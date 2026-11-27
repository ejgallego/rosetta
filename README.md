# Rosetta

Rosetta analyzes foreign-language code and named references through LSP. Its
core provides owned server sessions, versioned document handles, source
projection, typed failures, and portable analysis snapshots. It imports only
Lean and Std and has **no Lake package dependencies**.

Rosetta is developed independently at
[ejgallego/rosetta](https://github.com/ejgallego/rosetta).
The API is experimental; pin a commit when consuming it. No compatibility is
promised between revisions.

| Directory | Lake package | Lean library | Direct package dependencies |
| --- | --- | --- | --- |
| `.` | `rosetta` | `Rosetta` | None |
| `verso/` | `verso-rosetta` | `VersoRosetta` | `rosetta`, `verso` |

The bridge references the core through `..`, so both packages share one source
checkout. Core-only consumers do not fetch Verso. Neither package depends on
Blueprint. Both packages default to Lean 4.35.0, matching the Verso bridge's
dependency stack. The core also uses the module system for Lean 4.36 consumers;
the bridge has not been migrated to a Lean 4.36 dependency stack.

## Build and test

From this directory:

```sh
lake build
lake test
```

`lake test` runs the deterministic protocol, source-mapping, deadline,
cancellation, and cleanup tests. Tests need Python 3 for the stdio mock peer;
the library needs only Lean and Std. Opt-in acceptance against rust-analyzer and coq-lsp
uses pinned executable checksums and fixture revisions in
[`tests/real_provers/pins.json`](tests/real_provers/pins.json). It copies fixtures
to temporary projects and tests the public analysis API with repeated requests,
Unicode source, hover, definitions, semantic coloring, and valid and invalid
overlays. It checks cancellation and reuse on the same connection, then verifies
process cleanup.

Run the probe with Rust 1.89.0 and coq-lsp 0.2.4 with Rocq 9.1.0 on Linux x86-64.

```sh
python3 scripts/check_real_provers.py \
  --rust-analyzer "$(rustup which rust-analyzer)" \
  --rustc "$(rustup which rustc)" --cargo "$(rustup which cargo)" \
  --coq-lsp "$HOME/.opam/rocq-v9.1/bin/coq-lsp" \
  --rocq "$HOME/.opam/rocq-v9.1/bin/rocq" \
  --output /tmp/rosetta-real-provers-evidence
```

Pass the executable paths reported by `rustup which`. The runner checks
exact `--version` output and SHA-256 before launching a server. These hashes
identify the binaries from the tested installation. The pinned Rocq runtime
and standard library must accompany coq-lsp.
The runner copies the files listed in the fixture manifest into temporary
projects. Its evidence includes tool identity, analysis bundles, cleanup
reports, and a transport trace with request and cancellation IDs.
If a probe times out or the runner receives `SIGINT` or `SIGTERM`, the Linux
runner terminates the probe and reaps its remaining descendants before removing
the temporary projects. The output directory retains any evidence written by
that point.

The optional `RosettaProverProfiles` library supplies two profiles for an
existing member of a project. Pass a `Rosetta.ProverProfiles.Config` with the
client configuration, optional provenance, and a readiness timeout (60 seconds
by default) to `rustProfile config key document` or `coqProfile config key document`.

- The Rust profile enables native diagnostics and disables check-on-save, build
  scripts, and proc macros. It records the process-wide status that satisfied
  its quiescence check and pulls diagnostics for the admitted overlay. A
  qualified reference such as `arithmetic::step` selects `step`. A caller can
  set `ContextProfile.referenceQueryOffset` to choose the position in a more
  complex expression.
- The Rocq profile enables final diagnostic publications and disables eager
  intermediate ones. It waits for diagnostics for the admitted document
  version, including an empty set, then calls `coq/getDocument` for the
  terminal result. This preserves both successful completion and failure when
  Rocq reaches its error limit.

Both profiles forward cancellation through their waits and requests.

Consumers can share `verify_tools`, `copy_fixtures`, and the
`tool_environment(evidence)` context manager from the Python runner. Keep the
context manager open through all sessions so they can resolve the verified
executables and use the explicit `RUSTC` path.

The client uses stdio transport and UTF-16 LSP positions. Process-group cleanup
is currently validated on Linux and uses the standard Unix `kill` utility for
forced termination. Server commands and project configuration are caller-owned.

## Use Rosetta

A Lean Lake configuration can pin the standalone repository:

```lean
require rosetta from git "https://github.com/ejgallego/rosetta" @ "<commit>"
```

Use an exact commit compatible with your Lean version, then run `lake update
rosetta`. For local development, replace the Git dependency with
`require rosetta from "/path/to/rosetta"`.

Use `import Rosetta`. A generator that already owns an initialized language
server can call `Rosetta.analyzeWithClient client profile requests` for repeated
analyses in that session. To let Rosetta start and close a server for each used
context, call `Rosetta.analyze`. With default analysis options its type is
`Rosetta.analyze : Array ContextProfile → Array Request → EIO ClientError Bundle`.
For a named symbol, `client.symbolSource "rust" "compress" (container? := some "EdwardsPoint")`
returns the location from the workspace index and the text it selects.
Use `client.lookupSymbols name container?` to handle zero or multiple exact
matches. Some servers index just the name. To follow a reference at a known
position, use `document.definitions position`. Semantic tokens add coloring
when supported.
The caller configures the server and waits for indexing as needed.
Use `ClientConfig.retainedNotificationMethods` and `Client.latestNotification`
when a loading-state update must survive eviction from the notification journal.

See the [API guide](doc/API.md) for sessions, handles, error constructors, and
conversion to an application's `IO` boundary.

For Verso authoring and rendering, use the separate
[VersoRosetta package](verso/README.md). It includes a runnable
[analysis and offline replay example](verso/README.md#a-complete-runnable-workflow).

See [migration notes](doc/MIGRATION.md) for consumers of the earlier VBP
snapshot or the lean-mirror vendored patch, and [contributing](CONTRIBUTING.md)
for checks and supported toolchains.

The package is licensed under [Apache 2.0](LICENSE).
