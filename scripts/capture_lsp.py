#!/usr/bin/env python3
"""Relay real LSP traffic and record metadata once each frame is forwarded."""
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys


class Direction:
    def __init__(self, source, target, name):
        self.source = source
        self.target = target
        self.name = name
        self.pending = b''
        self.forwarded = bytearray()
        self.frame_end = None
        self.payload_start = None

    def record(self, data, log):
        self.forwarded.extend(data)
        while self.forwarded:
            if self.frame_end is None:
                boundary = self.forwarded.find(b'\r\n\r\n')
                if boundary < 0:
                    return
                fields = (line.split(b':', 1) for line in self.forwarded[:boundary].split(b'\r\n'))
                length = next(int(value) for field, value in fields if field.lower() == b'content-length')
                self.payload_start = boundary + 4
                self.frame_end = self.payload_start + length
            if len(self.forwarded) < self.frame_end:
                return
            message = json.loads(self.forwarded[self.payload_start:self.frame_end])
            del self.forwarded[:self.frame_end]
            self.frame_end = None
            entry = {'direction': self.name}
            for field in ('method', 'id'):
                if field in message:
                    entry[field] = message[field]
            params = message.get('params')
            if isinstance(params, dict):
                if message.get('method') == '$/cancelRequest':
                    entry['cancelId'] = params['id']
                document = params.get('textDocument', params)
                if isinstance(document, dict) and 'version' in document:
                    entry['version'] = document['version']
            log.write(json.dumps(entry, separators=(',', ':')) + '\n')


def relay(child, log):
    outgoing = Direction(sys.stdin.buffer, child.stdin, 'clientToServer')
    incoming = Direction(child.stdout, sys.stdout.buffer, 'serverToClient')
    with selectors.DefaultSelector() as selector:
        for direction in (outgoing, incoming):
            os.set_blocking(direction.source.fileno(), False)
            os.set_blocking(direction.target.fileno(), False)
            selector.register(direction.source, selectors.EVENT_READ, direction)
        while True:
            for key, event in selector.select():
                direction = key.data
                if event == selectors.EVENT_READ:
                    try:
                        direction.pending = os.read(key.fd, 65536)
                    except BlockingIOError:
                        continue
                    selector.unregister(direction.source)
                    if direction.pending:
                        selector.register(direction.target, selectors.EVENT_WRITE, direction)
                    elif direction is outgoing:
                        child.stdin.close()
                    else:
                        return
                else:
                    try:
                        count = os.write(key.fd, direction.pending)
                    except BlockingIOError:
                        continue
                    except BrokenPipeError:
                        if direction is incoming:
                            return
                        selector.unregister(direction.target)
                        child.stdin.close()
                        continue
                    direction.record(direction.pending[:count], log)
                    direction.pending = direction.pending[count:]
                    if not direction.pending:
                        selector.unregister(direction.target)
                        selector.register(direction.source, selectors.EVENT_READ, direction)


def main():
    trace, command = Path(sys.argv[1]), sys.argv[2:]
    with trace.open('a', buffering=1) as log:
        # Inherit the recorder's group, which Rosetta owns during normal close.
        with subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE) as child:
            try:
                relay(child, log)
            finally:
                # Propagate server EOF even if it closed stdout before exiting.
                sys.stdout.buffer.close()
                child.stdin.close()
                try:
                    child.wait(timeout=0.25)
                except subprocess.TimeoutExpired:
                    child.kill()
    return child.returncode


if __name__ == '__main__':
    raise SystemExit(main())
