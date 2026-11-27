"""Exercise the Linux acceptance harness without installing Lean or real provers."""
from contextlib import contextmanager
import json
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.dont_write_bytecode = True
sys.path.insert(0, str(ROOT / 'scripts'))
import check_real_provers as runner


def frame(message):
    payload = json.dumps(message).encode()
    return b'Content-Length: %d\r\n\r\n' % len(payload) + payload


CLIENT_FRAME = frame({'jsonrpc': '2.0', 'method': 'textDocument/didOpen',
                      'params': {'textDocument': {'version': 3, 'text': 'x' * 262144}}})
SERVER_FRAME = frame({'jsonrpc': '2.0', 'id': 7, 'result': 'y' * 262144})


def worker_command(role, *args):
    return [sys.executable, str(Path(__file__).resolve()), '--worker', role, *map(str, args)]


def worker(role, directory):
    marker = directory / 'ready.json'
    if role in ('duplex', 'closed-stdin'):
        if role == 'closed-stdin':
            os.close(0)
            marker.touch()
        time.sleep(0.2)
        sys.stdout.buffer.write(SERVER_FRAME)
        sys.stdout.buffer.flush()
        if role == 'duplex':
            return int(sys.stdin.buffer.read() != CLIENT_FRAME)
    elif role in ('timeout', 'interrupt'):
        try:
            with runner._supervisor() as check_interrupted:
                return runner._run_probe(worker_command('lean', directory), ROOT, os.environ,
                                         directory / 'probe.jsonl', directory / 'probe.stderr',
                                         check_interrupted, timeout=3 if role == 'timeout' else 15)
        except (subprocess.TimeoutExpired, InterruptedError) as error:
            print(error, file=sys.stderr)
            return 3
    elif role == 'lean':
        print('probe started', flush=True)
        child = subprocess.Popen(worker_command('recorder', directory), start_new_session=True)
        return child.wait()
    elif role == 'recorder':
        return subprocess.call(worker_command('prover', directory))
    elif role == 'prover':
        print('prover started', file=sys.stderr, flush=True)
        pid_file = marker.with_suffix('.tmp')
        pid_file.write_text(json.dumps([os.getppid(), os.getpid()]))
        pid_file.rename(marker)
        time.sleep(20)
    return 0


@contextmanager
def start(command):
    with subprocess.Popen(command, start_new_session=True, stdin=subprocess.PIPE,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE) as process:
        try:
            yield process
        finally:
            if process.poll() is None:
                os.killpg(process.pid, signal.SIGKILL)
                process.wait(timeout=5)


def await_file(path):
    deadline = time.monotonic() + 5
    while not path.exists():
        if time.monotonic() >= deadline:
            raise AssertionError(f'{path.name} was not created')
        time.sleep(0.01)


class HarnessChecks(unittest.TestCase):
    def capture(self, directory, peer):
        return start([sys.executable, str(ROOT / 'scripts/capture_lsp.py'),
                      str(directory / 'wire.jsonl'), *peer])

    def test_server_eof_while_client_stays_open(self):
        with tempfile.TemporaryDirectory() as tmp:
            peer = [sys.executable, '-c', 'raise SystemExit(17)']
            with self.capture(Path(tmp), peer) as process:
                self.assertEqual(process.wait(timeout=5), 17)
                self.assertEqual(process.stdout.read(), b'')

    def test_crossed_large_writes(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            with self.capture(directory, worker_command('duplex', directory)) as process:
                output, errors = process.communicate(CLIENT_FRAME, timeout=5)
                self.assertEqual(process.returncode, 0, errors)
                self.assertEqual(output, SERVER_FRAME)
            records = [json.loads(line) for line in (directory / 'wire.jsonl').read_text().splitlines()]
            self.assertCountEqual(records, [
                {'direction': 'clientToServer', 'method': 'textDocument/didOpen', 'version': 3},
                {'direction': 'serverToClient', 'id': 7},
            ])

    def test_closed_server_input_does_not_drop_pending_server_output(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            with self.capture(directory, worker_command('closed-stdin', directory)) as process:
                await_file(directory / 'ready.json')
                output, errors = process.communicate(CLIENT_FRAME, timeout=5)
                self.assertEqual(process.returncode, 0, errors)
                self.assertEqual(output, SERVER_FRAME)

    def test_unpinned_files_are_excluded(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            fixtures = directory / 'fixtures'
            shutil.copytree(runner.FIXTURES, fixtures)
            extra = fixtures / 'rust/.cargo/config.toml'
            extra.parent.mkdir()
            extra.write_text('[build]\nrustc-wrapper = "unverified-wrapper"\n')
            with patch.object(runner, 'FIXTURES', fixtures):
                projects = runner.copy_fixtures(directory / 'copied')
                self.assertTrue((projects['rust'] / 'src/lib.rs').is_file())
                self.assertFalse((projects['rust'] / '.cargo').exists())
                with self.assertRaises(FileExistsError):
                    runner.copy_fixtures(directory / 'copied')

    def test_timeout_and_signals_reap_separate_session(self):
        for sent in (None, signal.SIGINT, signal.SIGTERM):
            with self.subTest(sent=sent), tempfile.TemporaryDirectory() as tmp:
                directory = Path(tmp)
                marker = directory / 'ready.json'
                with start(worker_command('timeout' if sent is None else 'interrupt', directory)) as process:
                    await_file(marker)
                    pids = json.loads(marker.read_text())
                    try:
                        if sent is not None:
                            process.send_signal(sent)
                        _, errors = process.communicate(timeout=7)
                        self.assertEqual(process.returncode, 3, errors)
                        self.assertIn(b'timed out' if sent is None else sent.name.encode(), errors)
                        self.assertEqual((directory / 'probe.jsonl').read_text(), 'probe started\n')
                        self.assertEqual((directory / 'probe.stderr').read_text(), 'prover started\n')
                        for pid in pids:
                            self.assertFalse(Path(f'/proc/{pid}').exists(), f'descendant {pid} survived')
                    finally:
                        # Keep a failed regression from leaking its fake processes.
                        for pid in pids:
                            command_line = Path(f'/proc/{pid}/cmdline')
                            try:
                                if str(directory).encode() in command_line.read_bytes().split(b'\0'):
                                    os.kill(pid, signal.SIGKILL)
                            except ProcessLookupError:
                                pass
                            except FileNotFoundError:
                                pass


if __name__ == '__main__':
    if sys.argv[1:2] == ['--worker']:
        raise SystemExit(worker(sys.argv[2], Path(sys.argv[3])))
    unittest.main()
