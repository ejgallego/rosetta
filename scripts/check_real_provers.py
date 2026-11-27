#!/usr/bin/env python3
"""Opt-in actual LSP acceptance. No installation or writes to supplied projects.

This module's verify_tools and copy_fixtures are shared with rendering consumers.
Pin failures are errors; untested versions must never be silently accepted.
"""
import argparse
from contextlib import contextmanager
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile

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
        version = subprocess.check_output([str(path), '--version'], text=True).strip()
        digest = sha256(path)
        if version != pin['version'] or digest != pin['sha256']:
            raise RuntimeError(f'{name}: executable differs from tested pin: {version!r}, sha256={digest}')
        evidence[name] = dict(path=str(path), **pin)
    return evidence


def copy_fixtures(destination):
    """Verify pinned fixture content, then copy into a private caller temp root."""
    pins = json.loads((FIXTURES / 'pins.json').read_text())
    for relative, digest in pins['fixtures'].items():
        if sha256(FIXTURES / relative) != digest:
            raise RuntimeError(f'fixture differs from pin: {relative}')
    destination = Path(destination)
    for language in ('rust', 'rocq'):
        shutil.copytree(FIXTURES / language, destination / language)
    return {language: destination / language for language in ('rust', 'rocq')}


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


def main():
    args = parser().parse_args()
    tools = {name: getattr(args, name.replace('-', '_')) for name in
             ('rust-analyzer', 'rustc', 'cargo', 'coq-lsp', 'rocq')}
    evidence = verify_tools(tools)
    subprocess.run(['nice', '-n', '10', 'lake', 'build', 'rosetta-real-provers'], cwd=REPO, check=True)
    args.output.mkdir(parents=True, exist_ok=True)
    for language in ('rust', 'rocq'):
        (args.output / (language + '.wire.jsonl')).write_text('')
    (args.output / 'toolchain.json').write_text(json.dumps(evidence, indent=2) + '\n')
    with tool_environment(evidence) as env, tempfile.TemporaryDirectory(prefix='rosetta-real-provers-') as tmp:
        projects = copy_fixtures(tmp)
        for language, server in [('rust', 'rust-analyzer'), ('rocq', 'coq-lsp')]:
            result = subprocess.run([str(REPO / '.lake/build/bin/rosetta-real-provers'), language,
                                     evidence[server]['path'], str(projects[language]),
                                     str(REPO / 'scripts/capture_lsp.py'),
                                     str((args.output / (language + '.wire.jsonl')).resolve())],
                                    cwd=REPO, env=env, text=True, capture_output=True, timeout=180)
            (args.output / (language + '.jsonl')).write_text(result.stdout)
            (args.output / (language + '.stderr')).write_text(result.stderr)
            if result.returncode:
                raise RuntimeError(f'{language} acceptance failed ({result.returncode}): {result.stderr}')
    print(f'Real Rust and Rocq acceptance passed; evidence: {args.output.resolve()}')


if __name__ == '__main__':
    main()
