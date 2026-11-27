# Rosetta API and structured failures

Experimental APIs for the standalone Rosetta core and VersoRosetta bridge.
See the [package overview](../README.md) for installation, toolchain, and runtime requirements.

## The retained surface

`import Rosetta` exposes the Lean/Std core. Call `analyze profiles requests`
to start and close one language server for each context used in the request
list. If you already own an initialized client, pass it to `analyzeWithClient`
to run one profile's requests on that server.

```lean
import Rosetta
open Rosetta

def analyzeInSession (client : Client) (profile : ContextProfile)
    (requests : Array Request) (cancel : Std.CancellationToken) : EIO ClientError Bundle :=
  analyzeWithClient client profile requests {
    maxQueriesPerCodeBlock := 0
    requestTimeoutMs? := some 2000
    cancel? := some cancel }
```

Both calls return a portable `Bundle`. The request list describes authored
code blocks and references. The profile supplies the project document,
reference syntax, and a `collectDiagnostics` callback. For live errors, callers
can match on `ClientError`. Saved feature failures carry diagnostic text in
`Outcome.failed`.

### Session ownership

`analyzeWithClient` requires every request to use the profile's context key.
The profile's client configuration must match the initialized client's
configuration after resolving the project root. The composed text is sent
as an unsaved LSP overlay. A relative profile document path is resolved under
the initialized client's project root.

If the caller has that file open, Rosetta uses the URI from the caller's
original `didOpen`. For example, a profile naming `./src/lib.rs` can borrow
an overlay opened as `src/lib.rs`. A symlink to the file works the same way.
Rosetta compares resolved local paths, so separate hard-link names remain
distinct. An unsaved file can also be found through a symlink to its existing
directory. If the path includes a dangling symlink, opening it returns
`ClientError.invalidInput`. Use the target path to open the unsaved file.
The language and composed text must match. A mismatch returns
`ClientError.invalidInput` before Rosetta sends another `didOpen` or any
feature requests. After analysis the caller can keep using the borrowed
overlay. Rosetta sends `didClose` only for a document it opened itself.

Borrowing checks the document version again when the callback returns. To run
overlapping analyses on the same file, open the matching overlay yourself
before starting either analysis. A temporary overlay belongs exclusively to
the scope that opened it. A competing scope fails before its callback runs.
Both kinds of callback receive a query-only `Document`. Use
`document.sourceExcerpt range` inside the callback to read and highlight that
version. To edit a file, retain the `OwnedDocument` returned by
`client.openDocument`. Each successful `owner.change text` returns the owner
of the new version. An edit or close invalidates borrowers of the old version.

When the writer queue is full, a temporary scope waits up to the configured
client timeout to enqueue `didClose`. If that expires, analysis returns
`documentCleanup` and the client keeps the overlay reserved for cleanup.
The error carries the wire URI and version of that scope. Once the queue drains,
pass both directly to `client.retryDocumentClose uri version`. A later version of
the same URI stays open if the caller retries an old error.

### Analysis options

Both analysis calls accept an optional `AnalysisOptions` argument.

| Option | Effect |
| --- | --- |
| `maxQueriesPerCodeBlock` | Select up to this many eligible semantic token positions per code block for hover and definition requests. The default is 32. Zero still collects diagnostics and coloring, and authored references are queried separately. |
| `requestTimeoutMs?` | Set a fresh deadline in milliseconds for each semantic token, hover, or definition request. The default uses the client's request timeout. |
| `cancel?` | Check cancellation between phases and pass the token to the diagnostic callback and feature requests. A pre-cancelled call to `analyze` exits before launching a server. |

A profile supplies `collectDiagnostics document cancel?`. Forward `cancel?`
to diagnostic requests, `document.sourceExcerpt` when reading highlighted source,
and `client.waitForNotification` when waiting for server readiness. A profile
with its own polling loop must check the token in that loop. The profile chooses
its diagnostic deadline. Server initialization and cleanup use their own
lifecycle timeouts. Cancellation between phases reports
`ClientError.cancelled "analysis"`. A cancelled feature request reports its
LSP method. A document release failure preserves any original error inside
`ClientError.documentCleanup`.

A direct call to `Document.semanticTokens` with a pre-cancelled token returns
`ClientError.cancelled "textDocument/semanticTokens"` before choosing the
server's full or range method.

The client accepts a versioned diagnostic under the canonical local file URI
when the version matches the open overlay. An unversioned diagnostic must
arrive under the original wire URI.

### Client and document operations

| Entry point | Contract |
| --- | --- |
| `Client.withSession config action` | Run an `EIO ClientError` action and own cleanup on success/failure |
| `Client.start config` | Acquire a manually owned client; returns `EIO ClientError Client` |
| `client.request method params timeoutMs cancel?` | Workspace/session RPC; return the JSON result or throw `ClientError` |
| `client.notify method params` | Admit a workspace/session notification to the bounded queue |
| `client.latestNotification method` | Observe retained params for a configured general notification method (`BaseIO`) |
| `client.openDocument uri languageId text` | Return an editable `OwnedDocument` for the supplied URI. A second open of the same local file returns `invalidInput`, including through a path alias. |
| `client.withDocument uri languageId text action` | Own a scoped document or borrow an open overlay with matching language and text |
| `client.document? uri` | Return a query-only `Document` with its original wire URI, including when looking up a local file alias. |
| `document.withBorrowed languageId action text?` | Run a callback on this exact admitted version and keep its overlay open. Optionally require matching text |
| `document.text` | Read the exact text through a current versioned handle |
| `document.request method params timeoutMs cancel?` | RPC with matching URI/version, checked at admission and delivery |
| `document.sourceExcerpt range timeoutMs cancel?` | Read the selected text and semantic coloring from this version |
| `owner.change text` | Admit an edit and return the owner for the new version |
| `document.diagnostics` | Read the snapshot through a current handle |
| `owner.close` | Close the owned version |
| `client.retryDocumentClose uri version` | Retry release of a temporary scope whose close could not enter the writer queue |
| `client.close` | Return a shared `CloseReport` in `BaseIO`, including incomplete cleanup |

Fallible operations above use `EIO ClientError`. Deadlines and caller-owned
cancellation tokens remain optional request parameters. The client-wide
capabilities, stderr, and notification observations are `BaseIO` operations.
`document.client` is a plain accessor for session-wide readiness observations;
it does not replace versioned document admission.

A caller that manages its own session can inspect the close report, then use
`report.finish result` to combine it with the action’s `Except ClientError α`.
This preserves the original error if cleanup also failed, as `withSession` does. Workspace and document requests remain separate because
only the latter carry a versioned document handle.

Source composition (`compose`, `Projection.mapRange`), portable bundle
validation/matching (`Bundle.validate`, `Bundle.find`), and the Verso bridge
retain their existing responsibilities. The current `VersoRosetta.collect` and
`VersoRosetta.enrich` entry points operate on Manual parts and remain pure.

## Workspace symbols and source excerpts

To display a named symbol, pass its name and optional container separately.
The following call reads a Rust method after the server has indexed the project.

```lean
client.symbolSource "rust" "compress" (container? := some "EdwardsPoint")
```

The result is a `SourceExcerpt` containing the indexed `location`, the requested
`languageId`, and the text `pieces`. The server chooses the
range. An index entry spanning `MyType measure();` yields that declaration.
An entry spanning only `measure` yields only the name.

Use `client.lookupSymbols "compress" (container? := some "EdwardsPoint")`
for an array of distinct `Lsp.Location` values in server order. The array can
be empty or contain several locations. Rosetta sends the name to `workspace/symbol`
and filters candidates by exact name and optional server-reported
`containerName`. It requests `workspaceSymbol/resolve` only for matching
candidates with a deferred range. Duplicate hits at the same URI and range
count as one location. Omitting the container searches across all containers.
`client.lookupSymbol name container?` requires exactly one matching location.
Both calls work on the index alone, including for an unopened non-file URI.
For all search candidates, use `client.workspaceSymbols query`. Both search APIs
reject reversed ranges and reject a resolve response that changes the candidate's
name, kind, URI or previously supplied container.

For Go to Definition, call `document.definitions position` with the position
of the reference you want to follow. In `MyType measure();`, asking at the
start of the declaration can take you to the definition of `MyType`. Named
lookup therefore uses only the indexed location. The direct definition request
returns all targets from the server, including an empty array or several targets.

Use `document.sourceExcerpt range` when you have a document handle.
Use `client.sourceExcerpt languageId location` when you have an `Lsp.Location`.
When the caller has the document open, Rosetta borrows its current overlay and
checks that the language matches. A competing temporary scope returns
`ClientError.invalidInput`. A workspace symbol range carries no document
version, so an edit since indexing can change the text at that range. For an
unopened local file URI without a query or fragment, Rosetta reads it from disk
and opens a temporary document. Other URIs need an open overlay. A concurrent
edit or close that invalidates a borrowed version is reported at scope release.

Concatenating `pieces` reproduces the selected text, including Unicode and CR,
LF or CRLF line endings. Oversized UTF-16 columns on existing lines select the
end of those lines. Reversed ranges, missing lines and columns that split a
surrogate pair are protocol errors. `SourceExcerpt.location` retains the supplied
range. Rosetta requests range tokens using the clamped coordinates or uses full
tokens when range tokens are unavailable. It clips tokens at the excerpt boundaries. Missing token
support or a null token result yields plain text. A caller explicitly requesting
tokens with `document.semanticTokens range?` receives `ClientError.unsupported`
when the requested operation is unavailable.

Both excerpt helpers accept an optional `timeoutMs` and `cancel?` after their
range or location. These control the semantic token request. The deadline
defaults to the client's configured request timeout.

Exact lookup reports `ClientError.invalidInput` if the name or supplied
container is empty. `lookupSymbol` and `symbolSource` also report it when the
server returns zero or several distinct matching locations. Malformed replies,
server errors, timeouts, disconnections and source-reading failures propagate.

Initialization options remain caller-owned. For example, rust-analyzer needs
`workspace.symbol.search.kind = "all_symbols"` to include methods. Workspace
indexing readiness is also server-specific. For rust-analyzer, enable status
notifications and retain the latest status before starting the session.

```lean
Client.withSession { config with
    experimentalCapabilities := config.experimentalCapabilities.mergeObj
      (Json.mkObj [("serverStatusNotification", toJson true)])
    retainedNotificationMethods := config.retainedNotificationMethods.push "experimental/serverStatus" } fun client => do
  let _ ← client.waitForNotification "experimental/serverStatus" (fun status =>
    (status.getObjValAs? Bool "quiescent").toOption == some true) 30000
  client.symbolSource "rust" "compress" (container? := some "EdwardsPoint")
```

`Client.latestNotification method : BaseIO (Option Json)` returns the latest
params for a configured general notification method. `none` means no retained
value (including an unconfigured method); `some Json.null` means a notification
with null params arrived. Each configured method retains one value, including
updates received during initialization. A newer busy status replaces an older
ready status. The default retains no methods. There is no Rust-specific status
field or API in the core.

`Client.waitForNotification method predicate timeoutMs cancel?` requires that method in
`retainedNotificationMethods`; otherwise it fails with `ClientError.invalidInput`.
It returns the retained JSON params that satisfied the predicate, independent
of journal eviction. It reports `ClientError.cancelled method` when the
supplied token is cancelled and `ClientError.deadline` when the predicate
misses its deadline. The caller configures the method before startup and supplies
its readiness predicate and any experimental capabilities. The returned params
describe the observed status at the time of the wait.

`Client.notifications` independently retains the latest 512 general notifications
with an eviction count. Diagnostics remain in `Document.diagnostics`, outside
both general-notification stores. The generic API accepts server-reported symbol
names and containers without parsing language-specific syntax.

## Error contract

`ClientError` is the machine-facing failure interface:

| Constructor | Meaning |
| --- | --- |
| `cancelled method` | Caller cancellation |
| `deadline method` | The operation's monotonic deadline elapsed |
| `closed` | Closing/closed client rejected work |
| `staleDocument uri expected actual?` | Handle version differs; `none` means the URI is closed |
| `invalidInput detail` | Invalid local input, query ownership, plan, or saved-data validation |
| `queueFull` / `pendingFull` | Outbound admission or pending-request capacity exhausted |
| `unsupported feature` | Required synchronization/encoding is unsupported |
| `disconnected` | Transport EOF or retirement |
| `protocol detail` | Malformed framing, JSON-RPC, or server feature data |
| `io cause` | Original `IO.Error`, preserving its constructor and fields |
| `server code message data?` | A valid server error response; arbitrary integer codes survive |
| `documentCleanup uri version cause original?` | Scoped document release failed, retaining any primary failure |
| `cleanup report original?` | Incomplete scoped cleanup, retaining any original typed failure |

Absent server error data is `none`; explicit JSON null is `some Json.null`.
A successful JSON null result stays a successful result. A valid server error
does not poison the transport or cancel an already completed request. Malformed
responses fail the transport as protocol errors.

Normal `IO` actions lift into `EIO ClientError` using `ClientError.io`, including
inside session/profile callbacks. Converting back to `IO` is explicit at the
application boundary. For example, a command-line entry point can choose:

```lean
import Rosetta
open Rosetta

def analyzeIO (profiles : Array ContextProfile) (requests : Array Request) : IO Bundle :=
  (analyze profiles requests).toIO (IO.userError ∘ toString)
```

Library callers can instead catch and match `ClientError`, or use the standard
`EIO.toBaseIO` to obtain `Except ClientError α`. `EIO.asTask` preserves the error
type across concurrent operations. Rendered error wording is diagnostic text,
not a machine interface. The API remains subject to the project's normal
no-compatibility policy between revisions.

## Analysis, snapshots and rendering

There are four distinct operations:

- `lspCode` records authored source to analyze in a registered context.
- `lspRef` queries an expression in that context's composed scope, using the
  profile's language-specific wrapper. It does not search a workspace index.
- `lookupSymbols` finds all distinct indexed locations for an exact symbol name
  and optional container. `lookupSymbol` requires one location.
- `sourceExcerpt` presents an existing source range, using an open overlay when
  available. `symbolSource` combines lookup and excerpt in one call.

`analyze` degrades explicit optional-feature server errors into `Outcome.failed`
and unavailable capabilities into `Outcome.unsupported`. Transport, protocol,
version, cancellation, deadline, IO and cleanup failures abort with their typed
`ClientError`; a dead session cannot masquerade as a useful saved analysis.

`QuerySnapshot.definitions` preserves the ranges returned by the prover.
For a definition in the composed document, `analyze` maps its selection range
back to the request keys in `localTargets`. Source projection and authored
locations recognize CR, LF and CRLF line endings. A live diagnostic or
definition selection that extends past the end of a line is clamped to that
line. Diagnostics on generated text retain normalized coordinates in the
composed document.

Bundle revision 2 stores each request's diagnostics in request coordinates.
Use `Request.authoredLocations` to recover their locations in the authored file.
Live semantic tokens and replay share `validateSemanticSpans`: ranges must be
positive, single-line, ordered, non-overlapping and on valid UTF-16 boundaries.
Each `SourceRange` stores its immutable `SourceIndex`. To split a file into
many pieces, construct that index once and pass it to each slice. A second
version of the file gets its own index, even if the URI is the same.

`ContextProfile.provenance : Json` is caller-owned evidence copied into the
context observation. It can record project revision, server version and profile
identity. Rosetta does not verify these values or infer external project freshness.
Replay matches exact requests, including authored source URIs. Moving the author
checkout can require recompilation and reanalysis. A JSON-serializable bundle is
not a location-independent cache or a certificate that dependencies are unchanged.

The Manual bridge uses collision-free `(context, key)` logical identities and
Verso's occurrence IDs/page registry. Repeated inclusions share analysis but get
unique HTML IDs; local definition links select the first occurrence in traversal
order and work across pages. Other genres can supply their own ID attributes and
`localLink : String → Option String` to the generic renderer. No filesystem links
are emitted for external targets. Hover text is escaped before entering Verso's
shared hover table; successful analysis adds no per-block status paragraph.

Session close is bounded. The writer owns stdin exclusively and dropping it
signals EOF. `CloseReport.workers` records reader, writer, stderr and watchdog
completion sampled after cleanup; `workersStopped` is computed from those fields.
On Linux, close inspects the owned process group with procfs before reaping its
leader. Forced close and a stalled in-flight write both send `SIGKILL` to the
group, including descendants with redirected stdio. Close keeps the group ID
reserved until any signal sender has stopped. Failed inspection or termination
produces an incomplete report, as do escaped descendants that keep pipes open.
See [migration notes](MIGRATION.md).
