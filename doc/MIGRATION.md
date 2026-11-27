# Migration to the standalone repository

Rosetta now has one source repository, `ejgallego/rosetta`, containing the root
`rosetta` package and the nested `verso-rosetta` package. Lean imports remain
`Rosetta` and `VersoRosetta`. Package configuration examples are in the root
and bridge READMEs. Pin an exact standalone commit and remove the former
vendored/path dependency; avoid mixing copies of the same package.

## Earlier VBP snapshots

The implementation was exported from VBP `bc6c4f5af69ae8c174b93a73e68a781c0e68e073`.
The initial export preserved the core and bridge Lean sources byte for byte. The four commits since the first package extraction retain their
history and authorship, with paths relocated to this repository's root.
Earlier development remains in VBP history. See [origin](ORIGIN.md).

VBP owns its Blueprint-specific generator, offline rendering tests and browser
fixtures. This repository owns the protocol/lifecycle tests, mock peer, real
server fixture sources, and the pure Verso bridge collection smoke test.

## Vendored lean-mirror readiness patch

The patch at lean-mirror `8d1e2e7` exposed a Rust-specific `Client.serverStatus`.
Standalone Rosetta adopts the behavior through generic notification retention:

```lean
-- Configure before Client.start / Client.withSession:
retainedNotificationMethods := #["experimental/serverStatus"]
-- Replace client.serverStatus with:
client.latestNotification "experimental/serverStatus"
```

Keep `experimentalCapabilities.serverStatusNotification = true` in the Rust
facade, and keep its `quiescent` predicate. The getter returns `BaseIO (Option
Json)`. `none` means no retained value; `some Json.null` is a received null value.
One latest value per configured method survives journal eviction, including
messages received during initialization. Diagnostics remain on document handles.

`Client.waitForNotification` now requires the method to be configured for
retention before startup; otherwise it returns `ClientError.invalidInput`.
The generic core contains no Rust-specific status field. Readiness is a
server-specific observation, not a liveness check or a promise against later
indexing work. See [API](API.md) for the complete contract.

## Document boundary hardening (bundle revision 2)

Regenerate saved analysis bundles. Revision 1 is rejected: diagnostics no longer
serialize a second authored-location array, and the Manual bridge encodes logical
keys as JSON pairs rather than colon-joined strings. There is no legacy decoder.

- Call `document.semanticTokens range?`; remove the formerly required text argument.
- Read `snapshot.diagnostics` directly as `Array Lsp.Diagnostic`. Use
  `snapshot.request.authoredLocations diagnostic.range` for authored locations.
- Supply `ContextProfile.provenance` when project/server/profile evidence matters.
  It is retained without being interpreted as freshness validation.
- Use `client.withDocument` for a scoped admitted version and `client.document?`
  to borrow open overlays. Workspace helpers now preserve existing overlays.
- `CloseReport.workers` replaces the stored Boolean; `workersStopped` is a derived
  accessor. Code constructing reports should supply individual worker states.
- Generic renderer callers supply occurrence ID attributes and a local-link
  resolver. Manual callers continue through `collect` / `enrich`; Verso owns IDs
  and paths, including repeated inclusions and multi-page output.

Session-fatal failures now propagate from `analyze` rather than being converted
to a feature-failure string. Catch typed `ClientError` at the application boundary.
The earlier prototype's `.call`, `.withClient` and string-parsed IO errors remain
obsolete; use `.request`, `.client`, and constructor matching.

## Scoped documents and Linux session close

Supply the admitted `languageId` when constructing `OpenDocument`. Edits keep
that language. `withDocument` and workspace source helpers reject borrowing an
overlay with a different language ID with `ClientError.invalidInput`.

Borrowed scopes validate the version at release and leave the owner's overlay
in place. An edit or close during the callback produces
`ClientError.documentCleanup`. A callback error is preserved in `original?`.

On Linux, session close checks the owned process group through readable procfs
before reaping its leader. Descendants with redirected stdio require
termination and set `CloseReport.forced`. Inspection and termination errors
appear in `CloseReport.errors`.

## Named symbols and source excerpts

Pass the exact symbol name and optional server-reported container separately.
For example, replace `client.sourceDefinition "rust" "Container::measure" "::"`
with `client.symbolSource "rust" "measure" (container? := some "Container")`.
For an unqualified name, omit `container?`.

| Previous API | Replacement |
| --- | --- |
| `Client.workspaceDefinition` | Call `client.lookupSymbol name (container? := some container)` for the indexed `Lsp.Location`. Lookup no longer takes a language ID. |
| `Client.highlightDefinition` | Pass an `Lsp.Location` to `client.sourceExcerpt languageId`. For a `DefinitionTarget` named `target`, pass `{ uri := target.uri, range := target.range }`. |
| `Client.sourceDefinition` | Call `client.symbolSource languageId name` with an optional container for the source excerpt at the indexed range. |
| `HighlightedDefinition` | Both excerpt helpers return `SourceExcerpt`, with `location`, `languageId`, and `pieces`. Regenerate saved values that used the old JSON shape. |

The workspace index determines the extent of a named excerpt. If an application
needs Go to Definition, call `document.definitions position` at a known
reference and select the target to display. Missing token support gives a
plain excerpt. Explicit `document.semanticTokens` requests still report
unsupported operations.

Use `client.lookupSymbols name container?` when the caller wants to handle an
empty result or select from multiple exact matches. It returns distinct indexed
locations in server order. `lookupSymbol` and `symbolSource` require a single
location.

Source excerpts and live diagnostics clamp oversized UTF-16 columns to the
end of the line. Saved request spans must already have valid UTF-16 ranges.
An excerpt keeps the original supplied location. CR, LF and CRLF each count as
one line ending. When building requests directly, supply one authored origin
per LSP line, including the final empty line after a terminator.

## Composing source slices

Create one `SourceIndex.ofString text` for each version of an input. Pass that
value as `SourceRange.source` to every slice of the same text. For a whole
input, use `index.fullRange`. This lets a projection retain one line index
when a deindented block has a separate slice for each line.

For a diagnostic or definition range received from a server, use
`projection.mapServerRange range`. It returns the normalized range in the
composed document and the corresponding source locations. Use
`projection.mapRange range` for a range that already uses exact coordinates.
