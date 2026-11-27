#!/usr/bin/env python3
"""Opt-in actual LSP acceptance. No installation or writes to supplied projects.

This module's verify_tools and copy_fixtures are shared with rendering consumers.
Pin failures are errors; untested versions must never be silently accepted.
"""
import argparse
from contextlib import contextmanager
import ctypes
import hashlib
import json
import os
from pathlib import Path
import signal
import subprocess
import tempfile
import time

REPO = Path(__file__).resolve().parents[1]
FIXTURES = REPO / 'tests/real_provers'


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def verify_tools(tools):
    """Check explicit real executable paths against the checked-in exact pins."""
    pins = json.loads((FIXTURES / 'pins.json').read_text())
    evidence = {}
    for name, pin in pins['tools'].items():
        path = Path(tools[name]).resolve(strict=True)
        digest = sha256(path)
        if digest != pin['sha256']:
            raise RuntimeError(f'{name}: executable differs from tested pin: sha256={digest}')
        version = subprocess.check_output([str(path), '--version'], text=True, timeout=10).strip()
        if version != pin['version']:
            raise RuntimeError(f'{name}: version differs from tested pin: {version!r}')
        evidence[name] = dict(path=str(path), **pin)
    return evidence


def copy_fixtures(destination):
    """Copy only the fixture files listed in the pin manifest, verifying their bytes."""
    pins = json.loads((FIXTURES / 'pins.json').read_text())
    destination = Path(destination)
    files = {}
    for relative, digest in pins['fixtures'].items():
        contents = (FIXTURES / relative).read_bytes()
        if hashlib.sha256(contents).hexdigest() != digest:
            raise RuntimeError(f'fixture differs from pin: {relative}')
        files[destination / relative] = contents
    projects = {language: destination / language for language in ('rust', 'rocq')}
    for project in projects.values():
        if os.path.lexists(project):
            raise FileExistsError(f'fixture directory already exists: {project}')
    for project in projects.values():
        project.mkdir(parents=True)
    for path, contents in files.items():
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open('xb') as out:
            out.write(contents)
    return projects


@contextmanager
def tool_environment(evidence):
    """Yield an environment resolving all five executable names to verified tools.

    Keep this context open until all LSP sessions have closed. RUSTC is explicit
    because cargo may otherwise select a different compiler from inherited state.
    """
    with tempfile.TemporaryDirectory(prefix='rosetta-verified-tools-') as tmp:
        for name, tool in evidence.items():
            (Path(tmp) / name).symlink_to(tool['path'])
        env = dict(os.environ)
        env['PATH'] = tmp + os.pathsep + env.get('PATH', '')
        env['RUSTC'] = evidence['rustc']['path']
        yield env


def parser():
    result = argparse.ArgumentParser(description=__doc__)
    for name in ('rust-analyzer', 'rustc', 'cargo', 'coq-lsp', 'rocq'):
        result.add_argument('--' + name, required=True, type=Path)
    result.add_argument('--output', type=Path, required=True)
    return result


def _reap_descendants():
    """Terminate surviving descendants adopted by this single-purpose supervisor."""
    children = Path(f'/proc/self/task/{os.getpid()}/children')
    grace = time.monotonic() + 0.1
    deadline = grace + 5
    forced = False
    while True:
        try:
            pid, _ = os.waitpid(-1, os.WNOHANG)
        except ChildProcessError:
            return forced
        if pid:
            continue
        now = time.monotonic()
        if now >= deadline:
            raise RuntimeError('acceptance process descendants did not exit')
        if now >= grace:
            for pid in children.read_text().split():
                try:
                    # As a direct child it retains its PID until we reap it.
                    os.kill(int(pid), signal.SIGKILL)
                except ProcessLookupError:
                    continue
                forced = True
        time.sleep(0.01)


@contextmanager
def _supervisor():
    """Only use in the CLI, where all child processes belong to this runner."""
    # Rosetta starts the recorder in its own session. Adopt survivors as soon as
    # Lean exits so even a timeout can account for them before removing fixtures.
    pr_set_child_subreaper = 36
    libc = ctypes.CDLL(None, use_errno=True)
    if libc.prctl(pr_set_child_subreaper, 1, 0, 0, 0) != 0:
        error = ctypes.get_errno()
        raise OSError(error, os.strerror(error))
    received = None

    def interrupt(signum, _frame):
        nonlocal received
        received = signal.Signals(signum)

    def check_interrupted():
        if received is not None:
            raise InterruptedError(f'acceptance interrupted by {received.name}')

    previous = {sig: signal.signal(sig, interrupt) for sig in (signal.SIGINT, signal.SIGTERM)}
    try:
        yield check_interrupted
        check_interrupted()
    finally:
        for sig, handler in previous.items():
            signal.signal(sig, handler)


def _run_probe(command, cwd, env, stdout, stderr, check_interrupted, timeout=180):
    check_interrupted()
    deadline = time.monotonic() + timeout
    process = None
    with stdout.open('wb') as out, stderr.open('wb') as err:
        try:
            process = subprocess.Popen(command, cwd=cwd, env=env, stdout=out, stderr=err,
                                       start_new_session=True)
            while process.poll() is None:
                check_interrupted()
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise subprocess.TimeoutExpired(command, timeout)
                time.sleep(min(0.05, remaining))
        finally:
            if process is not None and process.poll() is None:
                process.kill()
                process.wait(timeout=5)
            forced = _reap_descendants()
    check_interrupted()
    if process.returncode == 0 and forced:
        raise RuntimeError('acceptance subprocess left live descendants after reporting success')
    return process.returncode


def main():
    args = parser().parse_args()
    tools = {name: getattr(args, name.replace('-', '_')) for name in
             ('rust-analyzer', 'rustc', 'cargo', 'coq-lsp', 'rocq')}
    evidence = verify_tools(tools)
    subprocess.run(['nice', '-n', '10', 'lake', 'build', 'rosetta-real-provers'], cwd=REPO, check=True)
    args.output.mkdir(parents=True, exist_ok=True)
    for language in ('rust', 'rocq'):
        for suffix in ('.jsonl', '.stderr', '.wire.jsonl'):
            (args.output / (language + suffix)).write_text('')
    (args.output / 'toolchain.json').write_text(json.dumps(evidence, indent=2) + '\n')
    with (tool_environment(evidence) as env,
          tempfile.TemporaryDirectory(prefix='rosetta-real-provers-') as tmp,
          _supervisor() as check_interrupted):
        projects = copy_fixtures(tmp)
        for language, server in [('rust', 'rust-analyzer'), ('rocq', 'coq-lsp')]:
            out = args.output / (language + '.jsonl')
            err = args.output / (language + '.stderr')
            status = _run_probe([str(REPO / '.lake/build/bin/rosetta-real-provers'), language,
                                 evidence[server]['path'], str(projects[language]),
                                 str(REPO / 'scripts/capture_lsp.py'),
                                 str((args.output / (language + '.wire.jsonl')).resolve())],
                                REPO, env, out, err, check_interrupted)
            if status:
                raise RuntimeError(f'{language} acceptance failed ({status}): {err.read_text(errors="replace")}')
    print(f'Real Rust and Rocq acceptance passed; evidence: {args.output.resolve()}')


if __name__ == '__main__':
    main()
