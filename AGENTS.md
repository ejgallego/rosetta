# Rosetta project notes

This is the independent Rosetta repository. The root Lake package provides
`Rosetta` using only Lean/Std. The nested `verso/` package provides `VersoRosetta`
and depends on the root through `..`. Keep the core independent of Verso,
Blueprint, particular languages, and consuming UI frameworks.

Read CONTRIBUTING.md before implementation. Use `lake build`, `lake test`, and
`(cd verso && lake build && lake test)` for changes affecting both libraries.
`python3 tests/check_packages.py` validates fresh Git consumers and package
isolation. Run long Lean/Lake commands at low priority when practical.

Start implementation in one linked worktree per branch or PR under `.worktrees/`.
Keep the root checkout as the stable base. Use
`python3 -m scripts.rosetta_harness create-worktree <name> --branch <branch>`
and `require-worktree` before implementation. Existing linked worktrees are
supported. Keep owner, summary and write-scope coordination notes local via
`worktree-claim`. Consult `worktree-list` before overlapping work. Git is the
worktree registry, and `branch-policy.json` records the default branch. Run long
Lean/Lake commands through `scripts/lean-low-priority` or the harness `validate`
entrypoint. Repository-tooling checks are `validate harness`.

Use Lean-style public commit and PR subjects with a type and an imperative
summary. The types are `feat`, `fix`, `doc`, `style`, `refactor`, `test`, `chore`,
and `perf`. Omit scopes. Run
`python3 -m scripts.rosetta_harness prepare-pr` for a local public title/body
scaffold before preparing a PR. Branches are local-only unless a push is
explicitly authorized. Do not remove another owner's worktree or overwrite
their coordination notes without coordinating with them.

Pin external dependencies. The default toolchain is Lean 4.35.0; core CI also
checks 4.36.0-rc2. There is no compatibility promise: update affected callers,
tests and doc/MIGRATION.md coherently. Do not add legacy adapters solely to
preserve an obsolete API. Keep one source of truth and avoid new inductives
unless needed. Use the human author's name in authorship headers.

Do not push, publish releases, or edit other repositories unless authorized by
the user. Do not infer authorization from historical VBP coordination notes.
