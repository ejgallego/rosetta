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

The test compiles foreign code/reference authoring and checks plan collection
without starting a language server or importing Blueprint.

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
