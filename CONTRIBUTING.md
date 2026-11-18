# Contributing

`branch-policy.json` records `main` as the default development branch. The core
has no Lake dependencies. The separate `verso/` package owns Verso authoring and
rendering. Keep language and host-specific policy in consuming applications or
profiles.

## Checks

On Linux with Python 3.9 or later and the pinned Lean installed, run the full
suite with the following command.

```sh
python3 -m scripts.rosetta_harness validate
```

This runs Python harness tests, `lake build` and `lake test` in both packages,
then `python3 tests/check_packages.py`. Use `validate harness`, `validate core`,
`validate verso`, or `validate packages` for a focused check. You can combine
selections. Lean/Lake commands run through `scripts/lean-low-priority`, which
uses `nice -n 10` when available. Set `ROSETTA_LEAN_NICENESS` between 0 and 19 to
adjust it. Individual builds can use `scripts/lean-low-priority lake build`
(or `../scripts/lean-low-priority lake build` from `verso/`).

The package check builds fresh Git consumers and checks dependency isolation.
It requires a complete Git checkout; use `git fetch --unshallow` first if the
repository was cloned with a depth limit. The Verso CI job fetches full history.
It uses temporary output and does not publish a repository. Real Rust and Rocq
probes are opt-in; the README gives fixture-based commands. Browser and Blueprint
rendering acceptance belongs to the consuming VBP project.

The package check requires a clean checkout because the fresh consumers build
the current committed revision. Commit changes before running `validate packages`
or the full validation. The other checks can run while editing.

CI checks the core on Lean 4.35.0 and 4.36.0-rc2. The Verso bridge is pinned to
the Lean 4.35.0 dependency stack. CI's 4.36 job changes only its working copy of
`lean-toolchain`; this does not establish bridge support for 4.36. Linux is the
validated process-cleanup platform. Do not claim Windows/macOS support without
checking process-group shutdown and descendant cleanup there.

There is no compatibility promise between revisions, including documented APIs
and serialized data. Update affected tests, examples and migration notes with
API changes. Preserve copyright and license notices.

## Local worktrees and coordination

Keep the root checkout stable and start implementation in one linked worktree
per branch and intended PR. Read-only review and validation may run at the root.
The harness uses Git and the Python standard library.

```sh
python3 -m scripts.rosetta_harness worktree-list
python3 -m scripts.rosetta_harness create-worktree lifecycle --branch fix/lifecycle --owner alice --summary 'repair session shutdown' --scope Rosetta/Lsp
cd .worktrees/lifecycle
python3 -m scripts.rosetta_harness require-worktree
python3 -m scripts.rosetta_harness worktree-status
```

`create-worktree` starts a new branch from the configured default branch, or
reuses a local branch that has no worktree. Use `--base <commit-or-ref>` only to
start a new branch elsewhere. Without `--branch`, it creates `wip/<name>`.
Prefer `feat/`, `fix/`, `docs/`, and `chore/` branch prefixes for reviewable work,
and `wip/` for local exploration. New branches have no automatic upstream.
Fetching and updating the stable base remain explicit maintainer operations.

Creation requires a clean source checkout, a free destination, a branch with
no checkout, and real directories for worktree storage. Worktree names contain
lowercase letters, digits and hyphens. `worktree-list` reads Git's worktree
registry, which includes worktrees created before the harness.

Optional owner, summary and relative write scopes live in untracked
`.worktrees/_meta/<name>.json`. Run `worktree-claim` in the linked worktree to
record its owner and expected write scope.

```sh
python3 -m scripts.rosetta_harness worktree-claim --owner alice --summary 'repair session shutdown' --scope Rosetta/Lsp
```

These notes are advisory. Consult the worktree list before overlapping work.
Agree on file ownership and coordinate directly with the recorded owner before
changing a claim. Release a finished claim by removing its local notes after
coordination. Do not remove unrelated worktrees or branches.
Keep coordination state and local validation transcripts out of public PRs.

## Preparing a PR locally

Follow Lean upstream's `type: summary` form. The public types are `feat`, `fix`,
`doc`, `style`, `refactor`, `test`, `chore`, and `perf`. Describe the area in
the lowercase, imperative summary. Omit scopes and trailing periods.

```text
fix: close LSP sessions after initialization failures
doc: explain package isolation checks
chore: add local worktree coordination
```

Run `prepare-pr` from a clean implementation worktree with committed changes.

```sh
python3 -m scripts.rosetta_harness prepare-pr
```

This prints a local title/body scaffold against the configured default branch.
For stacked changes, use `prepare-pr --base <previous-layer-branch-or-commit>`
to target the preceding PR. The base must resolve to an ancestor of the current
commit. The command uses the latest commit subject or accepts
`--title 'type: summary'`. Replace the `This PR …` cue with the opening paragraph
and follow the guidance from the worktree's GitHub PR template. Get authorization
before pushing branches or creating, publishing or merging PRs.

## Pull request descriptions

Use a concise, imperative Lean-style `type: summary` for PR titles and commit
subjects, such as `feat: support workspace symbol lookup`. Keep the title about
the change and leave out author or tool prefixes.

Open with a short paragraph beginning "This PR" that explains the problem and
what callers or maintainers will see after the change. Write it so a reader can
understand it without following an issue link or opening the diff. A paragraph
is enough for many PRs.

When a PR changes several independent behaviors, use one paragraph per
behavior or one bullet per behavior. Keep implementation details, edge cases,
and regression evidence with the behavior they explain. For example, a fix
for lone carriage returns belongs with document edits.

Include design rationale, migration steps, or evidence when they help review.
End with any action callers need to take, and link to
[migration notes](doc/MIGRATION.md) for longer instructions. Leave file
inventories and local work notes out. CI records routine checks. Mention a
regression test, measurement, or manual check when its result matters to
review, and flag any validation gap that affects confidence. Revise the
description when review changes the scope or behavior so the final text can
serve as a squash commit message.

Example title and body for a scoped-document fix

```markdown
fix: validate borrowed document scopes

This PR makes `Client.withDocument` borrow an open LSP overlay only when its
language and text match. Previously it could borrow an overlay opened in a
different language and return success even if the document was edited or
closed during the callback. It now reports `ClientError.documentCleanup` if
the borrowed version becomes stale during the callback.

The mock-server tests cover an overlay edit during source highlighting and
verify that cleanup preserves the caller's edited overlay.

Callers constructing `OpenDocument` records directly must supply `languageId`.
```
