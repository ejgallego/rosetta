#!/usr/bin/env python3
"""Transparent real-server transport recorder for opt-in acceptance only.

Records method/id metadata after forwarding each complete LSP frame. It does not
fabricate responses or delay traffic. All descendants stay in Rosetta's group.
"""
import json
import os
from pathlib import Path
import selectors
import subprocess
import sys


def main():
    trace, command = Path(sys.argv[1]), sys.argv[2:]
    child = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=None)
    selector = selectors.DefaultSelector()
    selector.register(sys.stdin.buffer, selectors.EVENT_READ, ('clientToServer', child.stdin))
    selector.register(child.stdout, selectors.EVENT_READ, ('serverToClient', sys.stdout.buffer))
    buffers = {'clientToServer': bytearray(), 'serverToClient': bytearray()}
    with trace.open('a', buffering=1) as log:
        while selector.get_map():
            for key, _ in selector.select():
                direction, target = key.data
                data = os.read(key.fileobj.fileno(), 65536)
                if not data:
                    selector.unregister(key.fileobj)
                    if direction == 'clientToServer':
                        child.stdin.close()
                    continue
                buffer = buffers[direction]
                buffer.extend(data)
                while True:
                    boundary = buffer.find(b'\r\n\r\n')
                    if boundary < 0:
                        break
                    length = next(int(line.split(b':', 1)[1]) for line in buffer[:boundary].split(b'\r\n')
                                  if line.lower().startswith(b'content-length:'))
                    end = boundary + 4 + length
                    if len(buffer) < end:
                        break
                    frame = bytes(buffer[:end])
                    del buffer[:end]
                    target.write(frame)
                    target.flush()
                    message = json.loads(frame[boundary + 4:])
                    entry = dict(direction=direction)
                    for field in ('method', 'id'):
                        if field in message:
                            entry[field] = message[field]
                    if message.get('method') == '$/cancelRequest':
                        entry['cancelId'] = message['params']['id']
                    log.write(json.dumps(entry, separators=(',', ':')) + '\n')
    return child.wait()


if __name__ == '__main__':
    raise SystemExit(main())
