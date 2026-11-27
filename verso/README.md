# VersoRosetta

VersoRosetta is the Verso authoring/rendering bridge for [Rosetta](../README.md).
This is a separate Lake package so the core can remain dependency-free.
Its direct dependencies are the sibling core package and Verso; its lockfile
pins the Lean 4.35.0 Verso dependency stack. It does not depend on Blueprint.

From this directory:

```sh
lake build
lake test
```

The tests compile foreign code/reference authoring and run the complete workflow
against the repository's deterministic Python mock. They check saved-data replay,
source origins, repeated IDs, cross-page links, Unicode text and escaped hover
content in generated HTML. The tests and mock example require Python 3.

## A complete runnable workflow

From this directory, run:

```sh
lake exe rosetta-workflow analyze ../tests/lsp/mock_server.py _out/workflow
lake exe rosetta-workflow replay _out/workflow/analysis.json _out/replayed
```

The first command collects the authored requests, configures a temporary mock
project, runs analysis, and saves `analysis.json` alongside `html-single/` and
`html-multi/`. The second command reads that bundle and renders the same compiled
document without starting a server. Serve either HTML directory over HTTP to
inspect its hover popups.

[RosettaWorkflow.lean](examples/RosettaWorkflow.lean) contains the authoring,
profile and generation stages. [WorkflowMain.lean](examples/WorkflowMain.lean)
handles the command line. Repeated inclusions share analysis while receiving
separate HTML IDs. The example waits for the mock's pull-diagnostic response.
For a real prover, supply its executable, project file, reference wrapper and
diagnostic readiness policy in `ContextProfile`.

Bundles match exact compiled requests, including their authored origins. Moving
or editing the example source can require a new analysis. The optional
`AnalysisOptions` controls query budgets, request deadlines and cancellation.
`analyzeWithClient` reuses a caller-owned initialized server for one profile.

To pin the bridge from a Lean Lake configuration:

```lean
require «verso-rosetta» from git "https://github.com/ejgallego/rosetta" @ "<commit>" / "verso"
```

Use the same commit for the core and bridge. The bridge resolves the core from
its parent directory within the Git checkout. For local development use
`require «verso-rosetta» from "/path/to/rosetta/verso"`.

`import VersoRosetta` registers the experimental `lspCode` block and `lspRef`
role for Manual documents. `VersoRosetta.collect` gathers inert requests;
`Rosetta.analyze` performs the explicit IO stage; `VersoRosetta.enrich` attaches
accepted results before ordinary HTML/TeX rendering. Compilation and offline
enrichment do not start servers.

Logical `(context, key)` identities cannot collide through punctuation. Repeated
inclusions share snapshots and receive distinct Verso IDs. Definition links use
Verso's page registry and select the first rendered occurrence, including in
multi-page output. The renderer remains usable by other genres through explicit
ID attributes and a local-link resolver.

Saved bundles use revision 2; regenerate older bundles. Replay matches exact
compiled requests, including authored file URIs, and does not verify external
project freshness. Profiles may record caller-owned provenance for review.

See the shared [API guide](../doc/API.md). Both packages are covered by the
repository's [Apache 2.0 license](../LICENSE).
