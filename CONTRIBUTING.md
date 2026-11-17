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
It requires a complete Git checkout; use `git fetch --unshallow` first if the
repository was cloned with a depth limit. The Verso CI job fetches full history.
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
API changes. Preserve copyright and license notices.

## Pull request descriptions

Use a concise, imperative Lean-style `type: summary` for PR titles and commit
subjects, such as `feat: support workspace symbol lookup`. Keep the title about
the change and leave out author or tool prefixes.

Open with a short paragraph beginning "This PR" that explains the problem and
what callers or maintainers will see after the change. Write it so a reader can
understand it without following an issue link or opening the diff. A paragraph
is enough for many PRs.

Add design rationale, caller migration steps, or evidence when they help a
reviewer assess the change. Link to [migration notes](doc/MIGRATION.md) for
longer instructions. Leave file inventories and local work notes out. CI
records routine checks. Mention a regression test, measurement, or manual
check when its result matters to review, and flag any validation gap that
affects confidence. Revise the description when review changes the scope or
behavior so the final text can serve as a squash commit message.

Example title and body for a scoped-document fix

```markdown
fix: validate borrowed document scopes

This PR makes `Client.withDocument` borrow an open LSP overlay only when its
language and text match. Previously it could borrow an overlay opened in a
different language and return success even if the document was edited or
closed during the callback. It now reports `ClientError.documentCleanup` if
the borrowed version becomes stale during the callback.

Callers constructing `OpenDocument` records directly must supply `languageId`.
The mock-server tests cover an overlay edit during source highlighting and
verify that cleanup preserves the caller's edited overlay.
```
