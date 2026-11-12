# Contributing

The default development branch is `main`. The core has no Lake dependencies;
the separate `verso/` package owns Verso authoring and rendering. Keep language
and host-specific policy in consuming applications or profiles.

## Checks

On Linux with Python 3 and the pinned Lean installed:

```sh
lake build
lake test
(cd verso && lake build && lake test)
python3 tests/check_packages.py
```

The last command builds fresh Git consumers and checks dependency isolation.
It uses temporary output and does not publish a repository. Real Rust and Rocq
probes are opt-in; the README gives fixture-based commands. Browser and Blueprint
rendering acceptance belongs to the consuming VBP project.

CI checks the core on Lean 4.35.0 and 4.36.0-rc2. The Verso bridge is pinned to
the Lean 4.35.0 dependency stack. CI's 4.36 job changes only its working copy of
`lean-toolchain`; this does not establish bridge support for 4.36. Linux is the
validated process-cleanup platform. Do not claim Windows/macOS support without
checking process-group shutdown and descendant cleanup there.

There is no compatibility promise between revisions, including documented APIs
and serialized data. Update affected tests, examples and migration notes with
API changes. Keep commit subjects in the form `feat: summary`, `fix: summary`,
`refactor: summary`, etc. Preserve copyright and license notices.
