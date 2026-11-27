/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import Rosetta
import Rosetta.Client
import Rosetta.Source

open Lean Rosetta

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def range (sl sc el ec : Nat) : Lsp.Range :=
  { start := ⟨sl, sc⟩, «end» := ⟨el, ec⟩ }

private def pureChecks : IO Unit := do
  let source := "😀alpha\r\n  βeta\n"
  check (byteOffset? source ⟨0, 1⟩ == none) "accepted half a surrogate pair"
  check (byteOffset? source ⟨0, 7⟩ == some 9) "incorrect UTF-16 end of line"
  check (byteOffset? source ⟨0, 8⟩ == none) "accepted CRLF as line content"
  check (byteOffset? source ⟨8, 0⟩ == none) "accepted nonexistent line"
  check (byteRange? source (range 1 2 0 0) == none) "accepted reversed range"
  let mixed := "😀a\rβ\r\nc\n"
  let index := SourceIndex.ofString mixed
  check (index.lineCount == 4 && index.endPosition == ⟨3, 0⟩) "wrong mixed-ending line count"
  let boundaries : Array (Lsp.Position × Nat) :=
    #[(⟨0, 3⟩, 5), (⟨1, 0⟩, 6), (⟨1, 1⟩, 8), (⟨2, 0⟩, 10), (⟨3, 0⟩, 12)]
  for (position, offset) in boundaries do
    check (index.byteOffset? position == some offset && index.position? offset == some position)
      s!"wrong mixed-ending byte conversion at {position}"
  check (index.position? 2 == none && index.position? 9 == none && index.position? 13 == none)
    "accepted a byte inside a UTF-8 character or CRLF, or past EOF"
  check (index.clampedByteRange? (range 0 99 1 99) == some (range 0 3 1 1, 5, 8) &&
    index.clampedByteRange? (range 0 99 0 100) == some (range 0 3 0 3, 5, 5) &&
    index.clampedByteRange? (range 0 100 0 99) == none &&
    index.clampedByteRange? (range 0 1 1 99) == none && index.clampedByteRange? (range 0 0 4 0) == none)
    "server range clamping hid invalid positions or lost end-of-line semantics"
  let mixedProjection ← IO.ofExcept <| compose #[.generated "pre\r\n",
    .source { uri := "mixed", source := index, range := range 0 2 3 0 }]
  let mixedMapped ← IO.ofExcept <| mixedProjection.mapRange (range 1 0 3 1)
  check (mixedMapped.locations.size == 1 && mixedMapped.locations[0]!.range == range 0 2 2 1)
    "projection mapping through mixed endings changed source coordinates"
  let sourceIndex := SourceIndex.ofString source
  let projection ← IO.ofExcept <| compose #[
    .generated "//\n", .source { uri := "source", source := sourceIndex, range := range 0 0 0 7 },
    .generated "\n", .source { uri := "source-two", source := sourceIndex, range := range 1 2 1 6 },
    .generated "\n}" ]
  check (projection.text == "//\n😀alpha\nβeta\n}") "projection changed source text"
  let mapped ← IO.ofExcept <| projection.mapRange (range 1 2 1 7)
  check (mapped.locations.size == 1 && !mapped.containsGenerated &&
    mapped.locations[0]!.range == range 0 2 0 7) "incorrect mapped source range"
  let cross ← IO.ofExcept <| projection.mapRange (range 1 6 2 2)
  check (cross.locations.size == 2 && cross.containsGenerated &&
    cross.locations[1]!.range == range 1 2 1 4 && cross.locations[1]!.uri == "source-two")
    "cross-piece mapping lost provenance"
  let point ← IO.ofExcept <| projection.mapRange (range 2 0 2 0)
  check (point.locations.size == 1 && point.locations[0]!.range == range 1 2 1 2)
    "empty range chose the preceding piece"
  check (!(projection.mapRange (range 1 1 1 2)).isOk) "mapped an invalid surrogate position"
  let (normalized, mapped) ← IO.ofExcept <| projection.mapServerRange (range 1 2 1 99)
  check (normalized == range 1 2 1 7 && mapped.locations[0]?.any (·.range == range 0 2 0 7) &&
    !(projection.mapRange (range 1 2 1 99)).isOk)
    "strict and server range projection did not distinguish oversized columns"
  let emptyIndex := SourceIndex.ofString ""
  let empty ← IO.ofExcept <| compose #[.source {
    uri := "empty-source", source := emptyIndex, range := emptyIndex.fullRange }]
  let emptyMapped ← IO.ofExcept <| empty.mapRange (range 0 0 0 0)
  check (emptyMapped.locations.size == 1 && emptyMapped.locations[0]!.uri == "empty-source")
    "empty authored fragment lost its source identity"
  let first := SourceIndex.ofString "😀"
  let second := SourceIndex.ofString "β"
  let seam ← IO.ofExcept <| compose #[
    .source { uri := "same", source := first, range := first.fullRange },
    .source { uri := "empty", source := emptyIndex, range := emptyIndex.fullRange },
    .source { uri := "same", source := second, range := second.fullRange },
    .source { uri := "end", source := emptyIndex, range := emptyIndex.fullRange }]
  let fragments ← IO.ofExcept <| seam.mapRange (range 0 0 0 3)
  check (fragments.locations.map (·.source.text) == #["😀", "β"])
    "projected slices mixed distinct source snapshots of the same URI or included empty slices"
  let boundary ← IO.ofExcept <| seam.mapRange (range 0 2 0 2)
  check (boundary.locations.size == 1 && boundary.locations[0]!.source.text == "β")
    "a point at a seam was assigned to an empty or preceding slice"
  let eof ← IO.ofExcept <| seam.mapRange (range 0 3 0 3)
  check (eof.locations.size == 1 && eof.locations[0]!.uri == "end")
    "an EOF point lost its final empty source segment"
  let targetRange := range 1 0 3 0
  let selection := range 1 4 1 8
  let targets ← IO.ofExcept <| definitionTargets <| .arr #[
    Json.mkObj [("uri", "file:///one"), ("range", toJson selection)],
    Json.mkObj [("targetUri", "file:///two"), ("targetRange", toJson targetRange),
      ("targetSelectionRange", toJson selection)]]
  check (targets.size == 2 && targets[1]!.range == targetRange &&
    targets[1]!.selectionRange == selection) "definition targets lost information"
  let provider := Json.mkObj [("legend", Json.mkObj [
    ("tokenTypes", toJson #["function"]), ("tokenModifiers", toJson #["declaration"])])]
  let validTokens := Json.mkObj [("data", toJson #[0, 2, 5, 0, 1])]
  let spans ← IO.ofExcept <| semanticSpans source provider validTokens
  check (spans.size == 1) "valid Unicode semantic token was rejected"
  for data in #[#[0, 1, 1, 0, 0], #[0, 2, 1, 9, 0], #[0, 2, 1, 0, 2], #[0, 2]] do
    check (!(semanticSpans source provider (Json.mkObj [("data", toJson data)])).isOk)
      "invalid semantic token was accepted"

private def bundleChecks : IO Unit := do
  let request : Request := {
    key := "body", context := "demo", kind := .code
    text := "abcd\n", origins := #[⟨"file:///authored", 10, 2⟩, ⟨"file:///authored", 11, 0⟩] }
  let bundle : Bundle := {
    requests := #[{ request, languageId := "test", tokens := .ok #[], diagnostics := #[], queries := #[] }]
    contexts := #[{
      key := "demo", languageId := "test", diagnosticCoverage := "test"
      completion := .null, provenance := Json.mkObj [("revision", toJson "caller-owned")], generatedDiagnostics := #[] }] }
  IO.ofExcept bundle.validate
  let some initial := bundle.requests[0]? | throw <| IO.userError "missing test snapshot"
  for spans in #[
      #[{ range := range 0 0 0 3, tokenType := "test", modifiers := #[] },
        { range := range 0 2 0 4, tokenType := "test", modifiers := #[] }],
      #[{ range := range 0 1 0 1, tokenType := "test", modifiers := #[] }],
      #[{ range := range 0 1 1 0, tokenType := "test", modifiers := #[] }],
      #[{ range := range 0 2 0 4, tokenType := "test", modifiers := #[] },
        { range := range 0 0 0 2, tokenType := "test", modifiers := #[] }]] do
    let saved := { initial with tokens := .ok spans }
    check (!({ bundle with requests := #[saved] }.validate).isOk)
      "saved bundle accepted invalid spans rejected by the live contract"
  check (!({ bundle with formatRevision := 1 }.validate).isOk) "accepted obsolete bundle schema"
  let oversized := { initial with diagnostics := #[
    { range := range 0 0 0 99, message := "oversized saved range" }] }
  check (!({ bundle with requests := #[oversized] }.validate).isOk)
    "bundle validation clamped a saved diagnostic range"
  let replay ← IO.ofExcept <| (fromJson? (toJson bundle) : Except String Bundle)
  check (toJson replay == toJson bundle) "bundle replay lost caller provenance"
  let mapped ← IO.ofExcept <| request.authoredLocations (range 0 1 0 3)
  check (mapped[0]?.any (·.range == range 10 3 10 5)) "authored diagnostic mapping is not derived correctly"

  let mixed := { request with text := "😀a\rβ\r\nc\n", origins := #[
    ⟨"file:///authored", 10, 2⟩, ⟨"file:///authored", 11, 0⟩,
    ⟨"file:///authored", 12, 1⟩, ⟨"file:///authored", 13, 0⟩] }
  let mapped ← IO.ofExcept <| mixed.authoredLocations (range 0 2 2 1)
  check (mapped.map (·.range) == #[range 10 4 10 5, range 11 0 11 1, range 12 1 12 2])
    "authored diagnostics used different line endings from source projection"
  let some initial := bundle.requests[0]? | throw <| IO.userError "missing test snapshot"
  let mixedSnapshot := { initial with request := mixed, tokens := .ok #[
    { range := range 1 0 1 1, tokenType := "test", modifiers := #[] }] }
  IO.ofExcept ({ bundle with requests := #[mixedSnapshot] }.validate)
  let invalid := { mixedSnapshot with tokens := .ok #[
    { range := range 1 0 1 2, tokenType := "test", modifiers := #[] }] }
  check (!({ bundle with requests := #[invalid] }.validate).isOk)
    "saved tokens accepted an oversized column"

private def awaitNotification (client : Client) (method : String) : IO Unit := do
  let deadline := (← IO.monoMsNow) + 2000
  repeat
    let (notifications, _) ← client.notifications
    if notifications.any (fun message =>
        (message.getObjValAs? String "method").toOption == some method) then return
    check ((← IO.monoMsNow) < deadline) s!"missing notification: {method}"
    IO.sleep 2

private def failsWith (action : EIO ClientError α) (accept : ClientError → Bool) : EIO ClientError Bool := do
  try
    discard action
    return false
  catch error => return accept error

private def heldParams (document : Document) (tag : String) : Json := Json.mkObj [
  ("textDocument", Json.mkObj [("uri", toJson document.uri)]), ("tag", toJson tag)]

/-- Deliberately create an escaped descendant, outside Rosetta's ownership, so
cleanup fails honestly. The test owns and kills that descendant in all cases. -/
private def cleanupFailureChecks (server : String) : EIO ClientError Unit := do
  for failAction in #[false, true] do
    let escaped ← IO.mkRef (none : Option Nat)
    try
      let result ← (Client.withSession
        { command := "python3", args := #[server], root := "." } fun client => do
          let value ← client.request "test/escaped-descendant" .null
          let pid ← EIO.ofExcept <| (fromJson? (α := Nat) value).mapError ClientError.protocol
          escaped.set (some pid)
          if failAction then discard <| client.request "test/error" .null
        ).toBaseIO
      match result with
      | .error (.cleanup report original?) =>
        check (report.reaped && !report.workersStopped && !report.complete)
          "incomplete cleanup falsely claimed successful resource ownership"
        if failAction then
          check (original? matches some (.server (-32097) "server-specific" none))
            "cleanup erased the original typed server error"
        else check original?.isNone "cleanup invented an original action error"
      | _ => check false "incomplete scoped cleanup did not return a structured failure"
    finally
      if let some pid ← escaped.get then
        let result ← IO.Process.output { cmd := "kill", args := #["-KILL", "--", toString pid] }
        check (result.exitCode == 0) "test-owned escaped descendant was not killed"

private def checkDescendantStopped (server : String) (value : Json) (operation : String)
    (timeoutMs : Nat := 0) : EIO ClientError Unit := do
  let pid ← EIO.ofExcept <| (value.getObjValAs? Nat "pid").mapError ClientError.protocol
  let group ← EIO.ofExcept <| (value.getObjValAs? String "group").mapError ClientError.protocol
  let started ← EIO.ofExcept <| (value.getObjValAs? String "started").mapError ClientError.protocol
  let result ← IO.Process.output {
    cmd := "python3", args := #[server, "--check-descendant", toString pid, group, started, toString timeoutMs] }
  check (result.exitCode == 0) s!"{operation}: {result.stderr}"

/-- Even when all three stdio handles are redirected, the owned descendant
must be dead before close completes. -/
private def detachedDescendantChecks (server : String) : EIO ClientError Unit := do
  unless System.Platform.isLinux do return
  let client ← Client.start { command := "python3", args := #[server], root := "." }
  let value ← try
    client.request "test/detached-descendant" .null
  catch error =>
    discard <| client.close
    throw error
  let before ← IO.monoMsNow
  let report ← client.close
  check (report.complete && report.forced)
    s!"leader exit left a descendant with detached stdio: {repr report}"
  check ((← IO.monoMsNow) - before < 3000) "detached descendant cleanup exceeded its deadline"
  checkDescendantStopped server value "close after leader exit"

private def stuckWriteDescendantChecks (server large : String) : EIO ClientError Unit := do
  unless System.Platform.isLinux do return
  for closeDuringWrite in #[true, false] do
    let client ← Client.start { command := "python3", args := #[server], root := "." }
    try
      let descendant ← client.request "test/detached-descendant" .null
      discard <| client.request "test/stop-reading" (toJson "observe-write")
      let pending ← EIO.asTask
        (client.request "test/blocked" (toJson large) (if closeDuringWrite then 10000 else 3000)) .dedicated
      awaitNotification client "test/write-started"
      check (!(← IO.hasFinished pending)) "oversized write finished before the peer stopped reading"
      if !closeDuringWrite then
        check ((← IO.wait pending) matches .error (.deadline _)) "stuck write missed its deadline"
        checkDescendantStopped server descendant "stuck-write watchdog before close" 2000
      let before ← IO.monoMsNow
      let report ← client.close
      check report.complete s!"stuck write left incomplete cleanup: {repr report}"
      check ((← IO.monoMsNow) - before < 3000) "stuck-write cleanup exceeded its deadline"
      if closeDuringWrite then
        check report.forced "close reported graceful cleanup while the write was stuck"
        checkDescendantStopped server descendant "close during a stuck write"
        check ((← IO.wait pending) matches .error .closed) "close did not interrupt the stuck write"
    finally
      discard <| client.close

private def localDefinitionChecks (server : String) : EIO ClientError Unit := do
  let profile : ContextProfile := {
    key := "demo", languageId := "test"
    client := {
      command := "python3", args := #[server], root := "."
      initializationOptions := Json.mkObj [("mode", toJson "local-definition")] }
    document := "/tmp/rosetta-analysis.mock"
    referencePrefix := "", referenceSuffix := ""
    collectDiagnostics := fun _ _ => pure { coverage := "mock", diagnostics := #[
      { range := range 0 0 0 99, message := "source" },
      { range := range 0 99 1 0, message := "generated" }] } }
  let declaration : Request := {
    key := "decl", context := profile.key, kind := .code
    text := "x", origins := #[⟨"authored", 0, 0⟩] }
  let exact := { declaration with key := "exact", kind := .reference }
  let oversized := { exact with key := "oversized" }
  let bundle ← analyze #[profile] #[declaration, exact, oversized]
  let snapshot ← IO.ofExcept <| bundle.find declaration
  check (snapshot.diagnostics.map (·.range) == #[range 0 0 0 1] &&
    bundle.contexts[0]?.any (fun context =>
      context.generatedDiagnostics.map (·.range) == #[range 0 1 1 0]))
    "analysis did not normalize live diagnostic columns before saving source and generated ranges"
  for (request, endCharacter) in #[(exact, 1), (oversized, 99)] do
    let snapshot ← IO.ofExcept <| bundle.find request
    let #[query] := snapshot.queries | check false s!"{request.key}: expected one reference query"
    let .ok #[target] := query.definitions | check false s!"{request.key}: expected one definition target"
    check (target.uri == (System.Uri.pathToUri profile.document : String) &&
      target.range == range 0 0 1 0 && target.selectionRange == range 0 0 0 endCharacter)
      s!"{request.key}: analysis changed the reported definition target"
    check (query.localTargets == #["decl"])
      s!"{request.key}: expected local definition target decl, got {repr query.localTargets}"

private def analysisFailureChecks (server : String) : EIO ClientError Unit := do
  for mode in #["server-error", "disconnect", "protocol"] do
    let profile : ContextProfile := {
      key := "demo", languageId := "test"
      client := {
        command := "python3", args := #[server], root := "."
        initializationOptions := Json.mkObj [("mode", toJson "workspace"), ("tokenMode", toJson mode)] }
      document := "/tmp/rosetta-analysis.mock"
      referencePrefix := "", referenceSuffix := ""
      collectDiagnostics := fun _ _ => pure { coverage := "mock", diagnostics := #[] } }
    let request : Request := {
      key := "body", context := "demo", kind := .code
      text := "x", origins := #[⟨"authored", 0, 0⟩] }
    let result ← (analyze #[profile] #[request]).toBaseIO
    check (match mode, result with
      | "server-error", .ok bundle => bundle.requests[0]?.any (fun snapshot => snapshot.tokens matches .failed _)
      | "disconnect", .error (.documentCleanup _ _ .disconnected (some .disconnected)) => true
      | "protocol", .error (.protocol _) => true
      | _, _ => false) s!"analysis erased or misclassified {mode}: {repr result.toOption.isSome}"

private def incrementalChangeChecks (server : String) : EIO ClientError Unit :=
  Client.withSession {
      command := "python3", args := #[server], root := "."
      initializationOptions := Json.mkObj [("mode", toJson "incremental")] } fun client => do
    let uri := "file:///incremental"
    let params := Json.mkObj [("textDocument", Json.mkObj [("uri", toJson uri)])]
    let initial := "a\rb"
    let mut document ← client.openDocument uri "test" initial
    check ((← document.request "test/document-text" params) == toJson initial)
      "didOpen changed the line endings"
    for text in #["a\r\nb\rc\n😀", "a\r", "a\r\n", "a\n", "", "😀"] do
      let oldText ← document.text
      document ← document.change text
      let serverText ← document.request "test/document-text" params
      check (serverText == toJson text)
        s!"incorrect incremental replacement of {repr oldText}: got {serverText.compress}"
      check ((← document.text) == text) "didChange changed the caller's text"
    document.close

private def mockChecks (server : String) : EIO ClientError Unit := do
  incrementalChangeChecks server
  let strictClient ← Client.start {
    command := "python3", args := #[server], root := "."
    initializationOptions := Json.mkObj [("mode", "strict-shutdown")] }
  let strictReport ← strictClient.close
  let lifecycleTrace ← strictClient.stderr
  check (lifecycleTrace == "shutdown\nexit\n")
    s!"mock did not observe shutdown then exit with absent params: {repr lifecycleTrace}"
  check (strictReport.complete && !strictReport.forced)
    s!"strict shutdown did not complete gracefully: {repr strictReport}"
  let eofClient ← Client.start {
    command := "python3", args := #[server], root := "."
    initializationOptions := Json.mkObj [("mode", toJson "wait-eof")] }
  let eofReport ← eofClient.close
  check (eofReport.complete && !eofReport.forced)
    s!"writer retained stdin after exit: {repr eofReport}"
  let mut chattyElapsed := 0
  let client ← Client.start { command := "python3", args := #[server], root := "." }
  try
    let uri := "file:///mock"
    let mut document ← client.openDocument uri "third-language" "x"
    discard <| client.request "test/barrier" .null
    let some diagnostic ← document.diagnostics | throw <| ClientError.io <| IO.userError "missing diagnostics"
    check (diagnostic.version? == some 1 && diagnostic.diagnostics.isEmpty)
      "diagnostics were appended, or stale diagnostics accepted"
    document ← document.change "y"
    discard <| client.request "test/barrier" .null
    let some diagnostic ← document.diagnostics | throw <| ClientError.io <| IO.userError "missing changed diagnostics"
    check (diagnostic.version? == some 2 && diagnostic.diagnostics.size == 1 &&
      diagnostic.diagnostics[0]!.message == "current warning") "incorrect edited diagnostics"
    discard <| client.request "test/unversioned" (Json.mkObj [("uri", toJson uri)])
    let some unversioned ← document.diagnostics | throw <| ClientError.io <| IO.userError "missing unversioned diagnostics"
    check (unversioned.version?.isNone) "invented a version for unversioned diagnostics"
    check (← failsWith (client.request "test/error" .null) (fun
      | .server code message data? => code == -32097 && message == "server-specific" && data?.isNone
      | _ => false)) "lost a server-specific error code"
    for data in #[Json.null, Json.mkObj [("detail", toJson #[1, 2]), ("nested", Json.null)]] do
      check (← failsWith (client.request "test/error" (Json.mkObj [
        ("code", toJson (739 : Int)), ("data", data)])) (fun
          | .server code message data? => code == 739 && message == "server-specific" && data? == some data
          | _ => false)) "structured server error data was lost"
    check ((← client.request "test/null" .null) == Json.null) "successful null result became an error"
    check ((← client.request "test/cancellations" .null) == toJson (#[] : Array Nat))
      "completed server errors emitted spurious cancellation notifications"
    check ((← client.request "test/client-request" .null) == toJson true)
      "unsupported server request was acknowledged as successful"
    discard <| client.request "test/stderr" .null
    check ((← client.stderr).utf8ByteSize ≤ 65536) "stderr exceeded retention limit"
    let old := document
    let held ← EIO.asTask (old.request "test/hold-document" (heldParams old "edit")) .dedicated
    awaitNotification client "test/held-edit"
    document ← document.change "z"
    check ((← IO.wait held) matches .error (.staleDocument _ _ _)) "edit delivered an obsolete response"
    check (← failsWith old.diagnostics (fun
      | .staleDocument actualUri expected actual? =>
        actualUri == uri && expected == old.version && actual? == some document.version
      | _ => false)) "old diagnostics handle was accepted"
    check (← failsWith (old.change "oops") (fun error => error matches .staleDocument _ _ _)) "stale edit was accepted"
    check (← failsWith (old.close) (fun error => error matches .staleDocument _ _ _)) "stale close was accepted"
    discard <| client.request "test/release-document" .null
    let old := document
    let held ← EIO.asTask (old.request "test/hold-document" (heldParams old "close")) .dedicated
    awaitNotification client "test/held-close"
    old.close
    document ← client.openDocument uri "third-language" "reopened"
    check (document.version > old.version) "reopening reused a document version"
    check ((← IO.wait held) matches .error (.staleDocument _ _ _)) "close/reopen delivered an obsolete response"
    discard <| client.request "test/release-document" .null
    check (← failsWith (old.request "test/barrier" (heldParams old "old")) (fun error => error matches .staleDocument _ _ _))
      "reopening revived an old handle"
    check (← failsWith (document.request "test/barrier" (Json.mkObj [
      ("textDocument", Json.mkObj [("uri", "file:///another")])])) (fun error => error matches .invalidInput _))
      "mismatched document URI was accepted"
    check (← failsWith (client.request "test/barrier" (heldParams document "unbound")) (fun error => error matches .invalidInput _))
      "unbound document query was admitted"
    for version in #[toJson (document.version - 1), toJson "invalid"] do
      check (← failsWith (document.request "test/die" (Json.mkObj [
        ("textDocument", Json.mkObj [("uri", toJson uri), ("version", version)])])) (fun error => error matches .invalidInput _))
        "mismatched or malformed document version was admitted"
    let first ← EIO.asTask (client.request "test/first" .null) .dedicated
    let heldDeadline := (← IO.monoMsNow) + 2000
    repeat
      let (notifications, _) ← client.notifications
      if notifications.any (fun message =>
          (message.getObjValAs? String "method").toOption == some "test/held") then break
      check ((← IO.monoMsNow) < heldDeadline) "first request was not admitted"
      IO.sleep 2
    let second ← client.request "test/second" .null
    let first ← EIO.ofExcept first.get
    check (first == toJson "first" && second == toJson "second") "responses were routed incorrectly"
    let before ← IO.monoMsNow
    let timedOut ← try
      discard <| client.request "test/chatter" .null 100
      pure false
    catch error => pure <| error matches .deadline _
    let elapsed := (← IO.monoMsNow) - before
    chattyElapsed := elapsed
    check (timedOut && elapsed < 1500) s!"chatty server defeated deadline ({elapsed}ms)"
    let precancelled ← Std.CancellationToken.new
    precancelled.cancel
    check (← failsWith (client.request "test/die" .null 1000 (some precancelled)) (fun error => error matches .cancelled _))
      "pre-cancelled request was admitted"
    check (← failsWith (client.request "test/die" .null 0) (fun error => error matches .deadline _)) "zero deadline was admitted"
    discard <| client.request "test/barrier" .null
    let cancel ← Std.CancellationToken.new
    let task ← EIO.asTask (client.request "test/chatter" .null 5000 (some cancel)) .dedicated
    IO.sleep 30
    cancel.cancel
    check ((← IO.wait task) matches .error (.cancelled "test/chatter")) "explicit cancellation was misclassified"
    check ((← client.request "test/barrier" .null) == toJson true) "late response contaminated next request"
  finally
    check (← client.close).complete "mock process was not reaped"
  let dying ← Client.start { command := "python3", args := #[server], root := "." }
  let dead ← try
    discard <| dying.request "test/die" .null
    pure false
  catch error => pure <| error matches .disconnected
  check dead "server death was not observed"
  check (← dying.close).complete "dead server was not reaped"
  let rejected ← try
    let wrong ← Client.start {
      command := "python3", args := #[server], root := "."
      initializationOptions := Json.mkObj [("mode", "wrong-encoding")] }
    discard <| wrong.close
    pure false
  catch error => pure <| error matches .unsupported _
  check rejected "incompatible position encoding was accepted"
  let reluctant ← Client.start {
    command := "python3", args := #[server], root := "."
    initializationOptions := Json.mkObj [("mode", "ignore-shutdown")] }
  let before ← IO.monoMsNow
  check (← reluctant.close).complete "server ignoring shutdown was not terminated and reaped"
  check ((← IO.monoMsNow) - before < 3000) "forced cleanup exceeded its deadline"
  let stubborn ← Client.start {
    command := "python3", args := #[server], root := "."
    initializationOptions := Json.mkObj [("mode", "ignore-term")] }
  let pending ← EIO.asTask (stubborn.request "test/chatter" .null 15000) .dedicated
  IO.sleep 30
  let before ← IO.monoMsNow
  let closeA ← EIO.asTask (show EIO ClientError CloseReport from stubborn.close) .dedicated
  let closeB ← EIO.asTask (show EIO ClientError CloseReport from stubborn.close) .dedicated
  IO.sleep 20
  check (← failsWith (stubborn.request "test/barrier" .null) (fun error => error matches .closed)) "closing admitted new work"
  check ((← IO.wait pending) matches .error .closed) "closing did not report its typed failure"
  let reportA ← EIO.ofExcept (← IO.wait closeA)
  let reportB ← EIO.ofExcept (← IO.wait closeB)
  let forcedElapsed := (← IO.monoMsNow) - before
  check (reportA.complete && reportA.forced && reprStr reportA == reprStr reportB)
    s!"concurrent cleanup did not share a successful report: {repr reportA}; {repr reportB}"
  check (reprStr (← stubborn.close) == reprStr reportA) "repeated cleanup changed its report"
  check (forcedElapsed < 3000) "SIGKILL cleanup exceeded its deadline"
  let inherited ← Client.start { command := "python3", args := #[server], root := "." }
  discard <| inherited.request "test/descendant" .null
  let inheritedReport ← inherited.close
  check (inheritedReport.complete && inheritedReport.forced)
    s!"leader exit left a descendant holding stdio: {repr inheritedReport}"
  detachedDescendantChecks server
  let large := String.ofList (List.replicate (2 * 1024 * 1024) 'x')
  stuckWriteDescendantChecks server large
  let blocked ← Client.start {
    command := "python3", args := #[server], root := ".", timeoutMs := 2000 }
  discard <| blocked.request "test/stop-reading" .null
  -- Encode/queue a notification first, so payload preparation cannot consume
  -- the query's deadline before backpressure has even been exercised.
  blocked.notify "test/blocked" (toJson large)
  IO.sleep 20
  let before ← IO.monoMsNow
  check (← failsWith (blocked.request "test/barrier" .null 120) (fun error => error matches .deadline _))
    "backpressure defeated the request deadline"
  let blockedElapsed := (← IO.monoMsNow) - before
  check (blockedElapsed < 1500) s!"blocked write took {blockedElapsed}ms"
  check (← blocked.close).complete "blocked writer/reader tasks were not stopped"
  let active ← Client.start {
    command := "python3", args := #[server], root := ".", timeoutMs := 2000 }
  discard <| active.request "test/stop-reading" .null
  let before ← IO.monoMsNow
  check (← failsWith (active.request "test/blocked" (toJson large) 500) (fun error => error matches .deadline _))
    "active write defeated its deadline"
  let activeElapsed := (← IO.monoMsNow) - before
  -- If encoding alone exhausted the deadline, notify would still be admitted.
  -- This assertion proves that the active frame actually retired the session.
  check (← failsWith (active.notify "test/after-interruption" .null) (fun
    | .disconnected | .deadline _ | .io _ => true
    | _ => false))
    "active write was not exercised or its interrupted transport was reused"
  check (activeElapsed < 1500) "active write deadline was unbounded"
  check (← active.close).complete "interrupted active writer was not stopped"
  let small ← Client.start { command := "python3", args := #[server], root := ".", maxQueuedBytes := 4096 }
  check (← failsWith (small.request "test/large" (toJson large)) (fun error => error matches .queueFull)) "outbound bytes were unbounded"
  check ((← small.request "test/barrier" .null) == toJson true) "rejected frame corrupted the transport"
  check (← small.close).complete "outbound capacity test did not clean up"
  let capped ← Client.start {
    command := "python3", args := #[server], root := "."
    maxPendingRequests := 1 }
  let pending ← EIO.asTask (capped.request "test/first" .null) .dedicated
  awaitNotification capped "test/held"
  check (← failsWith (capped.request "test/barrier" .null) (fun error => error matches .pendingFull)) "pending requests were unbounded"
  check (← capped.close).complete "capacity test did not clean up"
  check ((← IO.wait pending) matches .error .closed) "pending capacity test request survived close"
  for method in #["test/bad-envelope", "test/bad-error", "test/oversized", "test/partial"] do
    let broken ← Client.start { command := "python3", args := #[server], root := "." }
    let before ← IO.monoMsNow
    check (← failsWith (broken.request method .null 120) (fun
      | .deadline _ => method == "test/partial"
      | .protocol _ => method != "test/partial"
      | _ => false)) s!"malformed server succeeded: {method}"
    check ((← IO.monoMsNow) - before < 1500) "malformed input blocked its caller"
    check (← broken.close).complete s!"malformed server left IO workers: {method}"
  let scopedFailure ← failsWith (Client.withSession
    { command := "python3", args := #[server], root := "." }
    (fun _ => throw <| ClientError.invalidInput "original action failure") : EIO ClientError Unit)
    (fun | .invalidInput message => message == "original action failure" | _ => false)
  check scopedFailure "scoped cleanup lost the action failure"
  let cause := IO.Error.noFileOrDirectory "fixture-owned-input" 2 "preserved cause"
  check (← failsWith (do (throw cause : IO Unit)) (fun
    | .io (.noFileOrDirectory filename code details) =>
      filename == "fixture-owned-input" && code == 2 && details == "preserved cause"
    | _ => false)) "lifting IO erased its structured cause"
  let startup ← (Client.start { command := "/rosetta-missing-fixture/server", root := "." }).toBaseIO
  let startupTyped := match startup with
    | .error (.io _) | .error .disconnected => true
    | _ => false
  let startupDetail := match startup with | .error error => toString error | .ok _ => "unexpected success"
  check startupTyped s!"process startup failure escaped the typed interface: {startupDetail}"
  cleanupFailureChecks server
  IO.println <| (Json.mkObj [("mock", "passed"), ("chattyDeadlineMs", toJson chattyElapsed),
    ("blockedWriteDeadlineMs", toJson blockedElapsed), ("activeWriteDeadlineMs", toJson activeElapsed),
    ("forcedCloseMs", toJson forcedElapsed)]).compress

private def queryParams (uri : String) (line character : Nat) : Json := Json.mkObj [
  ("textDocument", Json.mkObj [("uri", toJson uri)]),
  ("position", toJson (⟨line, character⟩ : Lsp.Position))]

private def rocqDocument (document : Document) : EIO ClientError Json :=
  document.request "coq/getDocument" <| Json.mkObj [("textDocument", Json.mkObj [
    ("uri", toJson document.uri), ("version", toJson document.version)])]

private def pushDiagnostics (document : Document) :
    EIO ClientError (Array Lsp.Diagnostic) := do
  let deadline := (← IO.monoMsNow) + 3000
  repeat
    if let some snapshot ← document.diagnostics then
      if snapshot.version? == some document.version then return snapshot.diagnostics
    check ((← IO.monoMsNow) < deadline) "missing current-version push diagnostics"
    IO.sleep 2
  throw <| ClientError.io <| IO.userError "unreachable diagnostics loop"

private def pullDiagnostics (document : Document) : EIO ClientError (Array Lsp.Diagnostic) := do
  let report ← document.request "textDocument/diagnostic" <| Json.mkObj [
    ("textDocument", Json.mkObj [("uri", toJson document.uri)])]
  IO.ofExcept <| report.getObjValAs? (Array Lsp.Diagnostic) "items"

private def probe (language command root : String) : EIO ClientError Unit := do
  let root ← IO.FS.realPath root
  let file := root / if language == "rust" then "src/lib.rs" else "Probe.v"
  let text ← IO.FS.readFile file
  let uri := (System.Uri.pathToUri file : String)
  let client ← Client.start {
    command, root, timeoutMs := 15000
    initializationOptions := if language == "rust" then
      Json.mkObj [("checkOnSave", toJson false), ("cargo", Json.mkObj [("buildScripts", Json.mkObj [("enable", toJson false)])]),
        ("procMacro", Json.mkObj [("enable", toJson false)])]
    else Json.mkObj [("check_only_on_request", toJson false)] }
  try
    let mut document ← client.openDocument uri (if language == "rust" then "rust" else "coq") text
    -- Each bounded query is retried only for transient startup evidence in this
    -- experiment. The library itself has no success-by-retry policy.
    let (line, character) := if language == "rust" then (7, 32) else (6, 7)
    let mut result := Json.null
    let mut targets := #[]
    for _ in [:20] do
      try
        result ← document.request "textDocument/definition" (queryParams uri line character) 3000
        targets ← IO.ofExcept <| definitionTargets result
      catch _ => pure ()
      if !targets.isEmpty then break
      IO.sleep 100
    let hover ← document.request "textDocument/hover" (queryParams uri line character)
    let (blockLine, blockCharacter) := if language == "rust" then (4, 36) else (2, 20)
    let blockDefinition ← document.request "textDocument/definition" (queryParams uri blockLine blockCharacter)
    let blockTargets ← IO.ofExcept <| definitionTargets blockDefinition
    check (targets.any fun target => blockTargets.any fun block =>
      target.uri == block.uri && target.selectionRange == block.selectionRange)
      "block and named reference did not resolve to the same declaration"
    let caps ← client.capabilities
    let mut spans : Array SemanticSpan := #[]
    if let .ok provider := caps.getObjVal? "semanticTokensProvider" then
      let tokens ← document.request "textDocument/semanticTokens/full" <| Json.mkObj [
        ("textDocument", Json.mkObj [("uri", toJson uri)])]
      spans ← IO.ofExcept <| semanticSpans text provider tokens
      check (!spans.isEmpty) "server advertised semantic tokens but returned no spans"
    let mut completion := Json.null
    if language == "rocq" then completion ← rocqDocument document
    let baselineDiagnostics ← if language == "rust" then pullDiagnostics document
      else pushDiagnostics document
    check (!(baselineDiagnostics.any (·.severity? == some .error))) "valid authored block has errors"

    -- Analyze an unsaved invalid fragment in the real project. The prefix is
    -- generated context; only the fragment has an authored-source location.
    let invalid := if language == "rust" then "fn invalid( {\n"
      else "Check missing_name.\n"
    let invalidSource := SourceIndex.ofString invalid
    let projection ← IO.ofExcept <| compose #[.generated (text ++ "\n"),
      .source { uri := "verso-source://spike/invalid", source := invalidSource, range := invalidSource.fullRange }]
    document ← document.change projection.text
    let mut invalidCompletion := Json.null
    if language == "rocq" then invalidCompletion ← rocqDocument document
    let invalidDiagnostics ← if language == "rust" then pullDiagnostics document
      else pushDiagnostics document
    let invalidHasError := invalidDiagnostics.any (·.severity? == some .error)
    let mut mappedLocations := #[]
    for diagnostic in invalidDiagnostics do
      let (_, mapped) ← IO.ofExcept <| projection.mapServerRange diagnostic.range
      for location in mapped.locations do
        mappedLocations := mappedLocations.push <| Json.mkObj [
          ("uri", toJson location.uri), ("range", toJson location.range),
          ("message", toJson diagnostic.message)]
    document ← document.change text
    if language == "rocq" then discard <| rocqDocument document
    let restoredDiagnostics ← if language == "rust" then pullDiagnostics document
      else pushDiagnostics document
    check (!(restoredDiagnostics.any (·.severity? == some .error))) "error survived document restoration"

    let mut detachedDefinition := Json.null
    if language == "rust" then
      let detachedUri := (System.Uri.pathToUri (root / "Detached.rs") : String)
      let detached ← client.openDocument detachedUri "rust"
        "use rosetta_spike::step;\nfn probe() { let _ = step; }\n"
      detachedDefinition ← detached.request "textDocument/definition" (queryParams detachedUri 1 22)
    let (notifications, dropped) ← client.notifications
    let diagnostics ← document.diagnostics
    let output := Json.mkObj [
      ("language", toJson language), ("capabilities", caps),
      ("definition", result), ("targets", toJson targets), ("hover", hover),
      ("blockDefinition", blockDefinition), ("semanticSpans", toJson spans),
      ("baselineDiagnostics", toJson baselineDiagnostics), ("completion", completion),
      ("invalidCompletion", invalidCompletion), ("invalidDiagnostics", toJson invalidDiagnostics),
      ("mappedDiagnostics", toJson mappedLocations), ("restoredDiagnostics", toJson restoredDiagnostics),
      ("detachedDefinition", detachedDefinition),
      ("diagnostics", toJson diagnostics), ("notifications", toJson notifications),
      ("droppedNotifications", toJson dropped), ("stderr", toJson (← client.stderr))]
    IO.println output.compress
    check invalidHasError "invalid fragment produced no error"
    check (!mappedLocations.isEmpty) "real diagnostic did not map to the authored fragment"
    check (!targets.isEmpty) s!"{language}: no definition targets"
    check (hover != Json.null) s!"{language}: no hover"
  finally
    check (← client.close).complete s!"{language}: process was not reaped"


private def assertCheck (condition : Bool) (message : String) : EIO ClientError Unit :=
  unless condition do throw <| ClientError.invalidInput message

private def scopeRequest (client : Client) (method : String := "test/scope-state") : EIO ClientError Json := do
  let deadline := (← IO.monoMsNow) + client.config.timeoutMs
  repeat
    match ← (client.request method .null).toBaseIO with
    | .ok result => return result
    | .error .queueFull =>
      if (← IO.monoMsNow) ≥ deadline then throw .queueFull
      IO.sleep 2
    | .error error => throw error
  throw <| ClientError.protocol "unreachable scope request loop"

example (client : Client) (_owner : OwnedDocument) : EIO ClientError Unit :=
  client.withDocument "file:///example" "test" "" fun document => by
    fail_if_success exact document.change "changed" *> pure ()
    fail_if_success exact document.close
    fail_if_success exact (show OwnedDocument from { toDocument := document }).close
    fail_if_success exact (show OwnedDocument from { _owner with toDocument := document }).close
    exact pure ()

private def scopeOwnershipChecks (config : ClientConfig) (uri alias : String) : EIO ClientError Unit :=
  Client.withSession config fun client => do
    let overlayText := "overlay"
    let called ← IO.mkRef false
    let contender (_ : Document) : EIO ClientError Unit := do called.set true
    let rejectsScope := fun error : ClientError => error matches .invalidInput _
    let former ← client.withDocument alias "test" overlayText fun document => do
      check (← failsWith (client.withDocument uri "test" overlayText contender) rejectsScope)
        "a second scope accepted the first scope's temporary overlay"
      let some active ← client.document? uri | throw <| ClientError.protocol "missing scoped overlay"
      check (← failsWith (active.withBorrowed "test" contender) rejectsScope)
        "a lookup permitted borrowing an active temporary overlay"
      check (!(← called.get)) "a competing scope ran its callback on a temporary overlay"
      check (← failsWith (client.retryDocumentClose document.uri document.version) rejectsScope)
        "explicit close recovery closed an active temporary overlay"
      let requiresLifecycle := fun error : ClientError =>
        error matches .invalidInput "use document lifecycle operations"
      for method in #["textDocument/didOpen", "textDocument/didChange", "textDocument/didClose"] do
        check (← failsWith (document.request method (heldParams document "lifecycle")) requiresLifecycle)
          s!"a scoped query admitted the lifecycle method {method}"
        check (← failsWith (client.request method .null) requiresLifecycle)
          s!"a session query admitted the lifecycle method {method}"
      let requested := range 0 1 0 5
      let excerpt ← document.sourceExcerpt requested
      check (excerpt.languageId == "test" && excerpt.location == ⟨alias, requested⟩ &&
        excerpt.pieces.map (·.text) == #["verl"])
        "the scoped excerpt did not use the versioned overlay text and wire URI"
      -- The reply follows both diagnostics, including the stale one.
      let state ← scopeRequest client
      check (state.getObjValD "documents" == Json.mkObj [(alias, toJson overlayText)])
        "the peer did not receive the expected wire overlay"
      let some snapshot ← document.diagnostics | throw <| ClientError.protocol "missing canonical push diagnostics"
      check (snapshot.version? == some document.version &&
        snapshot.diagnostics.map (·.message) == #["current canonical warning"])
        "canonical push diagnostics did not preserve the current version"
      return document
    check ((← client.document? uri).isNone) "the temporary overlay survived scope release"
    let isStale := fun error : ClientError => error matches .staleDocument _ _ _
    check (← failsWith former.text isStale) "the released scope still exposed its text"
    let owner ← client.openDocument alias "test" overlayText
    check (← failsWith (former.sourceExcerpt (range 0 0 0 1)) isStale)
      "reopening the overlay revived the released scope's excerpt"
    check (← failsWith (client.retryDocumentClose owner.uri owner.version) rejectsScope)
      "explicit close recovery closed a caller-owned overlay"
    client.withDocument uri "test" overlayText fun first =>
      client.withDocument alias "test" overlayText fun second =>
        check (first.uri == alias && second.uri == alias &&
          first.version == owner.version && second.version == owner.version)
          "overlapping scopes did not share the caller's alias overlay"
    check ((← owner.text) == overlayText) "release changed the caller's shared overlay"
    owner.close

private def scopeQueueChecks (config : ClientConfig) (uri : String) (expire : Bool) : EIO ClientError Unit :=
  Client.withSession { config with maxQueuedMessages := 1, timeoutMs := 2500 } fun client => do
    let initial ← scopeRequest client
    let resume := IO.FS.writeFile (config.root / "resume") ""
    let writes ← IO.mkRef (#[] : Array (Task (Except ClientError Json)))
    let scopeVersion ← IO.mkRef (none : Option Int)
    let ping := fun params => client.request "test/scope-ping" params 15000
    try
      let result ← (client.withDocument uri "test" "first overlay" fun document => do
        scopeVersion.set (some document.version)
        discard <| scopeRequest client "test/scope-pause"
        let payload := String.ofList (List.replicate (2 * 1024 * 1024) 'x')
        let blocked ← EIO.asTask (ping (toJson payload)) .dedicated
        awaitNotification client "test/scope-write-started"
        -- Exactly one filler can take the only queue slot behind the stalled write.
        let first ← EIO.asTask (ping .null) .dedicated
        let second ← EIO.asTask (ping .null) .dedicated
        writes.set #[blocked, first, second]
        check ((← IO.waitAny [first, second]) matches .error .queueFull)
          "the stalled writer failed to saturate the outbound queue"
        if expire then throw <| ClientError.invalidInput "scoped callback failed"
        -- Leave the queue full as the callback finishes, then let the writer drain.
        discard <| IO.asTask (IO.sleep 125 *> resume) .dedicated
        ).toBaseIO
      let recovery? ← if expire then do
        let .error (.documentCleanup cleanupUri cleanupVersion .queueFull
            (some (.invalidInput "scoped callback failed"))) := result
          | throw <| ClientError.protocol "expired scoped release lost the queue or callback error"
        check (cleanupUri == uri && (← scopeVersion.get) == some cleanupVersion)
          "expired scoped release reported the wrong document identity"
        let some pending ← client.document? uri | throw <| ClientError.protocol "missing recoverable overlay"
        let rejectsScope := fun error : ClientError => error matches .invalidInput _
        check (← failsWith (client.withDocument uri "test" "first overlay" fun _ => pure ()) rejectsScope)
          "another scope borrowed an overlay awaiting close"
        check (← failsWith (pending.withBorrowed "test" fun _ => pure ()) rejectsScope)
          "a looked-up handle borrowed an overlay awaiting close"
        check (← failsWith pending.text (fun error => error matches .invalidInput "document is awaiting close"))
          "a looked-up handle exposed an overlay awaiting close"
        resume
        let stillOpen ← scopeRequest client
        check (stillOpen.getObjValD "documents" == Json.mkObj [(uri, "first overlay")])
          "the peer forgot the recoverable overlay before explicit close"
        check (← failsWith (client.retryDocumentClose cleanupUri (cleanupVersion + 1))
          (fun error => error matches .staleDocument _ _ _))
          "close recovery accepted the wrong overlay version"
        client.retryDocumentClose cleanupUri cleanupVersion
        pure (some (cleanupUri, cleanupVersion))
      else do
        check result.isOk "transient queue pressure prevented scoped close"
        pure none
      for task in ← writes.get do
        check (match ← IO.wait task with | .ok (.bool true) | .error .queueFull => true | _ => false)
          "the peer did not finish draining the stalled writer"
      check ((← scopeRequest client) == initial && (← client.document? uri).isNone)
        "scoped close left an overlay or changed the server process"
      client.withDocument uri "test" "second overlay" fun replacement => do
        if let some (cleanupUri, cleanupVersion) := recovery? then
          check (← failsWith (client.retryDocumentClose cleanupUri cleanupVersion) (fun error =>
            match error with
            | .staleDocument actual expected current? =>
              actual == cleanupUri && expected == cleanupVersion && current? == some replacement.version
            | _ => false)) "retrying the previous close affected the new overlay version"
        let state ← scopeRequest client
        check (state.getObjValD "documents" == Json.mkObj [(uri, "second overlay")])
          "the recovered session failed to open a new overlay for the same URI"
      check ((← scopeRequest client) == initial) "the second scoped close left an overlay open"
    finally
      resume

private def scopeChecks (server : String) : EIO ClientError Unit := do
  let server ← IO.FS.realPath server
  IO.FS.withTempDir fun temporary => do
    let root ← IO.FS.realPath temporary
    IO.FS.writeFile (root / "probe.lean") "on disk"
    let uri := System.Uri.pathToUri (root / "probe.lean")
    let alias := System.Uri.pathToUri (root / "./probe.lean")
    check (uri != alias) "the scope test requires different wire and canonical URIs"
    let config : ClientConfig := {
      command := "python3", args := #[server.toString], root
      initializationOptions := Json.mkObj [("mode", "scope")] }
    scopeOwnershipChecks config uri alias
    if System.Platform.isLinux then
      scopeQueueChecks config uri false
      IO.FS.removeFile (root / "resume")
      scopeQueueChecks config uri true

private def sourceExcerptInterruptionChecks (config : ClientConfig) (file : System.FilePath)
    (location : Lsp.Location) (text : String) : EIO ClientError Unit := do
  let method := "textDocument/semanticTokens/range"
  let request : Request := {
    key := "source", context := "workspace", kind := .code, text
    origins := #[⟨"authored", 0, 0⟩, ⟨"authored", 1, 0⟩] }
  let profile : ContextProfile := {
    key := request.context, languageId := "test-language", client := config, document := file
    referencePrefix := "", referenceSuffix := ""
    collectDiagnostics := fun document cancel? => do
      let excerpt ← document.sourceExcerpt location.range (cancel? := cancel?)
      check (excerpt.pieces.any (fun piece => piece.tokenType == "function" && piece.text == "measure"))
        "diagnostic excerpt lost semantic coloring"
      return { coverage := "excerpt", diagnostics := #[] } }
  Client.withSession config fun client => do
    let cancel ← Std.CancellationToken.new
    client.notify "test/hold-next-token" .null
    let run (options : AnalysisOptions) := analyzeWithClient client profile #[request] options
    let task ← EIO.asTask (run { maxQueriesPerCodeBlock := 0, cancel? := some cancel }) .dedicated
    awaitNotification client "test/held-highlight"
    discard <| client.request "test/barrier" .null
    cancel.cancel
    check (match ← IO.wait task with | .error (.cancelled actual) => actual == method | _ => false)
      "diagnostic excerpt did not forward cancellation to its in-flight request"
    check ((← client.document? location.uri).isNone) "cancelled diagnostic excerpt leaked its overlay"
    let cancellations ← IO.ofExcept <| (← client.request "test/cancellations" .null).getArr?
    check (cancellations.size == 1) "server did not receive diagnostic excerpt cancellation"
    discard <| client.request "test/release-document" .null

    client.notify "test/hold-next-token" .null
    let started ← IO.monoMsNow
    check (← failsWith (client.sourceExcerpt profile.languageId location (timeoutMs := 500))
      (fun error => match error with | .deadline actual => actual == method | _ => false))
      "workspace excerpt did not forward its request deadline"
    check ((← IO.monoMsNow) - started < 3000) "workspace excerpt used the default deadline"
    check ((← client.document? location.uri).isNone) "expired workspace excerpt leaked its overlay"
    discard <| client.request "test/release-document" .null

    let bundle ← run { maxQueriesPerCodeBlock := 0 }
    check (bundle.contexts[0]?.any (·.diagnosticCoverage == "excerpt"))
      "analysis did not reuse the session after excerpt interruption"

/-- The indexed declaration starts at a return type whose definition is in another file.
The preceding Unicode comment and the CRLF terminator exercise exact source slicing. -/
def workspaceChecks (server : String) : EIO ClientError Unit :=
  IO.FS.withTempFile fun indexHandle indexFile => IO.FS.withTempFile fun targetHandle targetFile => do
    let symbolText := "/* 😀 */ MyType measure();\r\n"
    indexHandle.putStr symbolText
    indexHandle.flush
    targetHandle.putStr "struct MyType {};\nMyType measure();\nMyType measure(int);\n"
    targetHandle.flush
    let sourceUri := (System.Uri.pathToUri indexFile : String)
    let targetUri := (System.Uri.pathToUri targetFile : String)
    let indexedLocation : Lsp.Location := { uri := sourceUri, range := range 0 9 0 26 }
    let container := some "Container"
    let workspaceConfig (uri : String) (options : List (String × Json)) : ClientConfig := {
        command := "python3", args := #[server], root := "."
        initializationOptions := Json.mkObj <| [
          ("mode", toJson "workspace"), ("sourceUri", toJson uri), ("targetUri", toJson targetUri)] ++ options }
    Client.withSession { workspaceConfig sourceUri [] with
        retainedNotificationMethods := #["test/status", "test/null-status", "missing/readiness"] } fun client => do
      assertCheck ((← client.latestNotification "test/status").any (fun params =>
        (params.getObjValAs? Bool "ready").toOption == some false))
        "notification sent during initialization was lost"
      let location ← client.lookupSymbol "measure" container
      assertCheck (location == indexedLocation) "lookup did not return the indexed location"
      assertCheck ((← client.lookupSymbols "measure" container) == #[indexedLocation])
        "exact workspace matches included a duplicate or another container"
      let secondLocation := { indexedLocation with range := range 0 10 0 26 }
      assertCheck ((← client.lookupSymbols "measure") == #[indexedLocation, secondLocation])
        "exact workspace matches omitted ambiguity or changed server order"
      assertCheck ((← client.lookupSymbols "missing").isEmpty)
        "exact workspace matches did not return an empty array for a missing symbol"
      assertCheck (← failsWith (client.lookupSymbol "missing")
        (fun error => error matches .invalidInput _)) "lookup accepted a missing symbol"
      assertCheck (← failsWith (client.lookupSymbol "measure")
        (fun error => error matches .invalidInput _)) "unqualified lookup accepted ambiguous addresses"
      assertCheck (← failsWith (client.lookupSymbol "" container)
        (fun error => error matches .invalidInput _)) "lookup accepted an empty symbol name"
      assertCheck (← failsWith (client.lookupSymbol "empty-container" (some ""))
        (fun error => error matches .invalidInput _)) "lookup accepted an empty supplied container"
      let literal ← client.lookupSymbol " literal::measure " (some " Parent::Container ")
      assertCheck (literal == indexedLocation) "lookup did not use the literal symbol name and container"
      assertCheck (← failsWith (client.lookupSymbol " literal::measure " (some "Parent::Container"))
        (fun error => error matches .invalidInput _)) "lookup accepted a different container spelling"
      let raw ← client.workspaceSymbols "raw-defer"
      assertCheck (raw.size == 1 && (raw[0]?).any (fun symbol =>
        symbol.name == "raw-deferred" && symbol.containerName?.isNone &&
          symbol.location.uri == sourceUri && symbol.location.range == range 0 0 0 5))
        "raw workspace search filtered or failed to resolve a fuzzy candidate"
      let deferred ← client.lookupSymbol "deferred" container
      assertCheck (deferred.uri == sourceUri && deferred.range == range 0 0 0 5)
        "lookup did not resolve the deferred exact match amid irrelevant candidates"
      assertCheck (← failsWith (client.lookupSymbol "deferred" (some "Other"))
        (fun error => error matches .protocol _)) "lookup hid a malformed resolution for a matching symbol"
      assertCheck (← failsWith (client.lookupSymbol "deferredMore" container)
        (fun error => error matches .server (-32097) _ _)) "lookup hid a server error for a matching symbol"
      for name in #["bad-name", "bad-container"] do
        assertCheck (← failsWith (client.lookupSymbol name container)
          (fun error => error matches .protocol _)) s!"lookup hid an invalid workspace symbol identity: {name}"
      for name in #["bad-uri", "bad-range"] do
        assertCheck (← failsWith (client.lookupSymbol name container)
          (fun error => error matches .protocol _)) s!"lookup hid an invalid workspace symbol location: {name}"
      for name in #["bad-deferred-kind", "bad-deferred-container", "bad-deferred-tags"] do
        assertCheck (← failsWith (client.lookupSymbols name) (fun error => error matches .protocol _))
          s!"exact lookup failed to reject a malformed deferred symbol before resolving it: {name}"
        assertCheck (← failsWith (client.workspaceSymbols name) (fun error => error matches .protocol _))
          s!"raw lookup failed to reject a malformed deferred symbol before resolving it: {name}"
      for name in #["change-name", "change-kind", "change-container", "change-uri", "reverse-range",
                    "reversed-location"] do
        assertCheck (← failsWith (client.lookupSymbols name) (fun error => error matches .protocol _))
          s!"exact lookup accepted an inconsistent workspace symbol: {name}"
        assertCheck (← failsWith (client.workspaceSymbols name) (fun error => error matches .protocol _))
          s!"raw lookup accepted an inconsistent workspace symbol: {name}"
      let completed ← client.workspaceSymbols "complete-container"
      assertCheck (completed.size == 1 && completed[0]?.any (·.containerName? == some "Other"))
        "resolution could not supply a previously absent container"
      assertCheck ((← client.lookupSymbols "complete-container" (some "Other")).isEmpty)
        "exact lookup matched a container that was absent before resolution"
      let remoteUri := sourceUri.replace "file:///" "file://authority/"
      assertCheck (← failsWith (client.sourceExcerpt "test-language" { indexedLocation with uri := remoteUri })
        (fun error => error matches .unsupported _)) "a remote authority could read a local path"
      let traffic ← client.request "test/workspace-traffic" .null
      assertCheck (traffic == Json.mkObj [("documentOpens", toJson 0), ("definitionRequests", toJson 0)])
        s!"workspace lookup sent document traffic: {traffic}"
      let source ← client.symbolSource "test-language" "measure" container
      let excerpt ← client.sourceExcerpt "test-language" indexedLocation
      assertCheck (toJson source == toJson excerpt) "symbol source differed from the excerpt at the indexed location"
      assertCheck (source.location == indexedLocation && source.languageId == "test-language" &&
        String.join (source.pieces.toList.map HighlightedPiece.text) == "MyType measure();")
        "symbol source did not preserve the indexed declaration"
      assertCheck (source.pieces.any (fun piece => piece.tokenType == "function" &&
          piece.text == "measure" && piece.modifiers == #["declaration"])) "semantic coloring was lost"
      assertCheck ((← client.document? sourceUri).isNone) "reading the indexed source left its document open"
      let traffic ← client.request "test/workspace-traffic" .null
      assertCheck ((traffic.getObjValAs? Nat "definitionRequests").toOption == some 0)
        s!"reading source sent a definition request: {traffic}"
      client.withDocument sourceUri "test-language" symbolText fun document => do
        let scopedExcerpt ← document.sourceExcerpt indexedLocation.range
        assertCheck (toJson scopedExcerpt == toJson excerpt)
          "the scoped excerpt lost the workspace range or semantic coloring"
        let #[definition] ← document.definitions indexedLocation.range.start
          | throw <| ClientError.invalidInput "explicit navigation lost the type definition target"
        assertCheck (definition.uri == targetUri && definition.range == range 0 0 0 17 &&
          definition.selectionRange == range 0 7 0 13 &&
          definition.originSelectionRange? == some (range 0 9 0 15)) "type definition link ranges were ignored"
        let typeSource ← client.sourceExcerpt "test-language" { uri := definition.uri, range := definition.range }
        assertCheck (String.join (typeSource.pieces.toList.map HighlightedPiece.text) == "struct MyType {};")
          "explicit navigation did not lead to the type declaration"
        let #[first, second] ← document.definitions ⟨0, 16⟩
          | throw <| ClientError.invalidInput "explicit navigation lost multiple definition targets"
        assertCheck (first.uri == targetUri && first.range == range 1 7 1 14 &&
          second.uri == targetUri && second.range == range 2 7 2 14)
          "explicit navigation changed the locations of the overloaded declarations"
      -- Workspace helpers must prefer an admitted overlay and borrow it.
      let overlayText := "/* 😀 */ MyType collect();\r\n"
      let overlay ← client.openDocument sourceUri "test-language" overlayText
      check (← failsWith (overlay.semanticTokens (sourceIndex? := some (SourceIndex.ofString symbolText)))
        (fun error => error matches .invalidInput _))
        "token decoding accepted the disk index for a different unsaved overlay"
      assertCheck ((← client.lookupSymbol "measure" container) == indexedLocation)
        "workspace lookup depended on the overlay contents"
      check (← failsWith (client.symbolSource "another-language" "measure" container)
        (fun error => error matches .invalidInput "cannot change a borrowed document language"))
        "source highlighting accepted an overlay with a different language"
      let overlaid ← client.symbolSource "test-language" "measure" container
      assertCheck (String.join (overlaid.pieces.toList.map HighlightedPiece.text) == "MyType collect();")
        "workspace helper ignored the open overlay"
      assertCheck ((← overlay.text) == overlayText) "workspace helper closed a borrowed document"
      let borrowed ← client.withDocument sourceUri "test-language" overlayText fun document => pure document.version
      assertCheck (borrowed == overlay.version) "identical overlay was reopened"
      let mismatch ← (client.withDocument sourceUri "test-language" "other" fun _ => pure ()).toBaseIO
      assertCheck (match mismatch with | .error (.invalidInput _) => true | _ => false)
        "scoped helper replaced a borrowed overlay"
      check (← failsWith (client.withDocument sourceUri "another-language" overlayText fun _ => pure ())
        (fun error => error matches .invalidInput "cannot change a borrowed document language"))
        "scoped helper accepted identical text with a different language"
      client.notify "test/hold-next-token" .null
      let highlighting ← EIO.asTask (client.symbolSource "test-language" "measure" container) .dedicated
      awaitNotification client "test/held-highlight"
      let edited ← overlay.change overlayText
      let stale ← IO.wait highlighting
      assertCheck (match stale with
        | .error (.documentCleanup uri version (.staleDocument _ expected actual?)
            (some (.staleDocument ..))) =>
          uri == sourceUri && version == overlay.version && expected == version && actual? == some edited.version
        | _ => false) "workspace highlighting failed to release the stale borrowed version"
      discard <| client.request "test/release-document" .null
      check (← failsWith (client.withDocument sourceUri "another-language" overlayText fun _ => pure ())
        (fun error => error matches .invalidInput "cannot change a borrowed document language"))
        "editing an overlay lost its admitted language"
      client.withDocument sourceUri "test-language" overlayText fun document =>
        assertCheck (document.version == edited.version) "scoped helper failed to borrow the edited version"
      edited.close
      client.withDocument sourceUri "test-language" overlayText fun _ => pure ()
      assertCheck ((← client.document? sourceUri).isNone) "owned scoped document was not closed"
      for edit in #[false, true] do
        for failAction in #[false, true] do
          let owner ← client.openDocument sourceUri "test-language" overlayText
          let updated ← IO.mkRef (none : Option OwnedDocument)
          let result ← (client.withDocument sourceUri "test-language" overlayText fun borrowed => do
            if edit then updated.set (some (← owner.change "changed overlay")) else owner.close
            assertCheck (← failsWith borrowed.text (fun error => error matches .staleDocument _ _ _))
              "the borrowed view survived a mutation by its owner"
            if failAction then throw (α := Unit) <| ClientError.invalidInput "borrowed action failed"
            ).toBaseIO
          let current? ← client.document? sourceUri
          assertCheck (match result with
            | .error (.documentCleanup uri version (.staleDocument _ expected actual?) original?) =>
              uri == sourceUri && version == owner.version && expected == version && actual? == current?.map (·.version) &&
              (if failAction then original? matches some (.invalidInput "borrowed action failed")
               else original?.isNone)
            | _ => false) "borrowed release lost version invalidation or the primary error"
          if edit then
            let some current := current? | throw <| ClientError.invalidInput "borrowed release closed owner's overlay"
            assertCheck ((← current.text) == "changed overlay") "borrowed release changed owner's overlay"
            let some edited ← updated.get | throw <| ClientError.invalidInput "missing edited owner handle"
            edited.close
          else assertCheck current?.isNone "borrowed release reopened a closed document"
      let owner ← client.openDocument sourceUri "test-language" overlayText
      let failedBorrow ← (client.withDocument sourceUri "test-language" overlayText fun _ =>
        throw (α := Unit) <| ClientError.invalidInput "borrowed action failed").toBaseIO
      assertCheck (failedBorrow matches .error (.invalidInput "borrowed action failed"))
        "successful borrowed release changed the primary error"
      assertCheck ((← owner.text) == overlayText) "borrowed release closed the owner's valid overlay"
      owner.close
      let clipped ← client.sourceExcerpt "test-language" { indexedLocation with range := range 0 10 0 17 }
      assertCheck (String.join (clipped.pieces.toList.map HighlightedPiece.text) == "yType m" &&
        clipped.pieces.any (fun piece => piece.text == "yType" && piece.tokenType == "type") &&
        clipped.pieces.any (fun piece => piece.text == "m" && piece.tokenType == "function"))
        "clipping an excerpt lost text or semantic coloring at a source boundary"
      let multiline ← client.sourceExcerpt "test-language" { indexedLocation with range := range 0 0 1 0 }
      assertCheck (String.join (multiline.pieces.toList.map HighlightedPiece.text) == symbolText)
        "source slice lost Unicode or its CRLF terminator"
      let traffic ← client.request "test/workspace-traffic" .null
      assertCheck ((traffic.getObjValAs? Nat "definitionRequests").toOption == some 2)
        s!"workspace helpers sent additional definition requests: {traffic}"
      discard <| client.request "test/readiness" (Json.mkObj [])
      client.waitForNotification "test/status" (fun params =>
        (params.getObjValAs? Bool "ready").toOption == some true) 1000
      let (notifications, dropped) ← client.notifications
      assertCheck (notifications.size == 512 && dropped > 0) "notification trace was not bounded"
      assertCheck (!(notifications.any fun message =>
        (message.getObjValAs? String "method").toOption == some "test/status"))
        "readiness regression did not evict the journal entry"
      assertCheck ((← client.latestNotification "test/null-status") == some Json.null)
        "null notification params were confused with an absent notification"
      discard <| client.request "test/not-ready" (Json.mkObj [])
      let busy ← (client.waitForNotification "test/status" (fun params =>
        (params.getObjValAs? Bool "ready").toOption == some true) 0).toBaseIO
      assertCheck (match busy with | .error (.deadline _) => true | _ => false)
        "an older ready notification hid a newer busy notification"
      let timedOut ← (client.waitForNotification "missing/readiness" (fun _ => true) 0).toBaseIO
      assertCheck (match timedOut with | .error (.deadline _) => true | _ => false)
        "readiness wait did not respect its deadline"
      let unconfigured ← (client.waitForNotification "unconfigured/status" (fun _ => true) 0).toBaseIO
      assertCheck (match unconfigured with | .error (.invalidInput _) => true | _ => false)
        "readiness wait accepted an unconfigured notification method"
      IO.println "Indexed workspace source, explicit definitions, and Unicode highlighting passed"
    sourceExcerptInterruptionChecks (workspaceConfig sourceUri []) indexFile indexedLocation symbolText
    let uri := "untitled:workspace-symbol"
    let virtualLocation := { indexedLocation with uri }
    for definitionProvider in #[Json.bool true, Json.bool false, Json.null] do
      Client.withSession (workspaceConfig uri [
          ("definitionProvider", definitionProvider), ("tokenMode", "none")]) fun client => do
        assertCheck ((← client.lookupSymbol "measure" container) == virtualLocation)
          s!"lookup could not return an unopened virtual symbol with definition provider {definitionProvider}"
        let readSource := client.symbolSource "test-language" "measure" container
        assertCheck (← failsWith readSource (fun error => error matches .unsupported _))
          "the source helper hid the unsupported URI"
        let traffic ← client.request "test/workspace-traffic" .null
        assertCheck (traffic == Json.mkObj [("documentOpens", toJson 0), ("definitionRequests", toJson 0)])
          s!"an unopened virtual symbol sent document traffic: {traffic}"
        client.withDocument uri "test-language" symbolText fun document => do
          let source ← document.sourceExcerpt virtualLocation.range
          assertCheck (source.location == virtualLocation && source.languageId == "test-language" &&
            source.pieces.map (·.text) == #["MyType measure();"])
            "a temporary virtual excerpt failed to use its admitted text"
        let traffic ← client.request "test/workspace-traffic" .null
        assertCheck (traffic == Json.mkObj [("documentOpens", toJson 1), ("definitionRequests", toJson 0)])
          s!"reading an excerpt through its scoped view reopened the virtual document: {traffic}"
        let overlay ← client.openDocument uri "test-language" symbolText
        let source ← readSource
        assertCheck (source.location == virtualLocation && source.pieces.size == 1 &&
          source.pieces[0]!.text == "MyType measure();" &&
          source.pieces[0]!.tokenType.isEmpty && source.pieces[0]!.modifiers.isEmpty)
          "the source helper lost the plain text of the virtual overlay"
        assertCheck ((← overlay.text) == symbolText) "reading a virtual symbol closed its overlay"
        overlay.close
        let traffic ← client.request "test/workspace-traffic" .null
        assertCheck (traffic == Json.mkObj [("documentOpens", toJson 2), ("definitionRequests", toJson 0)])
          s!"reading a virtual symbol sent unexpected document traffic: {traffic}"
    for tokenMode in #["range", "full", "null", "none", "absent"] do
      Client.withSession (workspaceConfig sourceUri [("tokenMode", toJson tokenMode)]) fun client => do
        assertCheck ((← client.latestNotification "test/status").isNone)
          "retained state leaked into another session"
        let source ← client.symbolSource "test-language" "measure" container
        assertCheck (String.join (source.pieces.toList.map HighlightedPiece.text) == "MyType measure();")
          s!"{tokenMode}: source slice changed"
        assertCheck ((source.pieces.any (fun piece => piece.tokenType == "function")) ==
          (tokenMode == "range" || tokenMode == "full")) s!"{tokenMode}: token result was mishandled"
        if tokenMode == "none" || tokenMode == "absent" then
          let wholeRange := range 0 0 1 0
          let plain ← client.sourceExcerpt "test-language" { indexedLocation with range := wholeRange }
          assertCheck (plain.pieces.size == 1 && plain.pieces[0]!.text == symbolText &&
            plain.pieces[0]!.tokenType.isEmpty && plain.pieces[0]!.modifiers.isEmpty)
            "unsupported tokens changed Unicode or CRLF in the exact excerpt"
          assertCheck (← failsWith (client.sourceExcerpt "test-language" {
            indexedLocation with range := range 0 100 0 99 }) (fun error => error matches .protocol _))
            "unsupported tokens hid a reversed source range"
          client.withDocument sourceUri "test-language" symbolText fun document => do
            assertCheck (← failsWith (document.semanticTokens (some wholeRange))
              (fun error => error matches .unsupported _)) "the direct token request lost its capability error"
        if tokenMode == "range" then
          client.withDocument sourceUri "test-language" symbolText fun document => do
            assertCheck (← failsWith document.semanticTokens (fun error => error matches .unsupported _))
              "range-only server received a full-document request"
    Client.withSession (workspaceConfig sourceUri [("tokenMode", "range"),
        ("expectedTokenRange", toJson (range 0 26 1 6))]) fun client => do
      let mixedText := symbolText.replace "\r\n" "\r" ++ "😀tail\n"
      let clientLocation := { indexedLocation with range := range 0 99 1 99 }
      let overlay ← client.openDocument sourceUri "test-language" mixedText
      try
        let excerpt ← client.sourceExcerpt "test-language" clientLocation
        assertCheck (excerpt.location == clientLocation &&
          String.join (excerpt.pieces.toList.map HighlightedPiece.text) == "\r😀tail")
          "clamping a server location lost the original range or changed the exact source slice"
        for bad in #[range 0 4 0 26, range 0 0 3 0, range 0 100 0 99] do
          assertCheck (← failsWith (client.sourceExcerpt "test-language" { clientLocation with range := bad })
            (fun error => error matches .protocol _)) "invalid server range reached the token request"
      finally overlay.close
    for failureMode in #["server-error", "disconnect", "protocol", "deadline"] do
      Client.withSession { workspaceConfig sourceUri [("tokenMode", toJson failureMode)] with
          timeoutMs := 1000 } fun client => do
        let result ← (client.symbolSource "test-language" "measure" container).toBaseIO
        let detail := match result with | .ok _ => "success" | .error error => toString error
        assertCheck (match result with
          | .error error =>
            match failureMode, error with
            | "server-error", .server (-32601) _ _ => true
            | "disconnect", .documentCleanup _ _ .disconnected (some .disconnected) => true
            | "protocol", .protocol _ => true
            | "deadline", .deadline _ => true
            | _, _ => false
          | .ok _ => false) s!"symbol source lost the {failureMode} token error: {detail}"
    IO.println "Virtual overlays, semantic token negotiation, and typed failures passed"

private def analysisBudgetChecks (client : Client) (profile : ContextProfile) (code reference : Request) :
    EIO ClientError Unit := do
  let text := String.intercalate " " (List.replicate 40 "sample")
  let many := { code with text, origins := #[⟨"authored", 0, 0⟩] }
  let second := { many with key := "second" }
  let budgets : Array (AnalysisOptions × Nat) := #[({ maxQueriesPerCodeBlock := 0 }, 0),
    ({ maxQueriesPerCodeBlock := 2 }, 2), ({}, 32), ({ maxQueriesPerCodeBlock := 40 }, 40)]
  for (options, budget) in budgets do
    let before ← client.request "test/analysis-state" .null
    let previous ← IO.ofExcept <| before.getObjValAs? (Array Json) "requests"
    let result ← analyzeWithClient client profile #[many, second, reference] options
    let columns := (Array.range budget).map (· * 7)
    for request in #[many, second] do
      let snapshot ← IO.ofExcept <| result.find request
      check (match snapshot.tokens with | .ok spans => spans.size == 40 | _ => false)
        "query budget changed the semantic coloring"
      check (snapshot.queries.map (·.range) == columns.map (fun start => range 0 start 0 (start + 6)))
        s!"query budget {budget} did not select the first eligible tokens in each block"
    let ref ← IO.ofExcept <| result.find reference
    check (ref.queries.size == 1) "code query budget suppressed an explicit reference"
    let after ← client.request "test/analysis-state" .null
    let requests ← IO.ofExcept <| after.getObjValAs? (Array Json) "requests"
    let positions : Array Lsp.Position := columns.map (fun column => ⟨0, column⟩) ++
      columns.map (fun column => ⟨1, column⟩) ++ #[⟨2, 6⟩]
    let expected := positions.flatMap fun position =>
      #[(toJson "textDocument/hover", toJson position), (toJson "textDocument/definition", toJson position)]
    let calls := (requests.extract previous.size requests.size).map fun request =>
      (request.getObjValD "method", request.getObjValD "position")
    check (calls == #[(toJson "textDocument/semanticTokens/full", .null)] ++ expected)
      s!"query budget {budget} sent unexpected feature requests"

private def checkAnalysisOverlay (overlay : Document) (text : String) : EIO ClientError Unit := do
  check ((← overlay.text) == text) "analysis closed or changed the caller overlay"
  let params := Json.mkObj [("textDocument", Json.mkObj [("uri", toJson overlay.uri)])]
  check ((← overlay.request "test/document-text" params) == toJson text)
    "analysis closed or changed the caller overlay on the server"

private def createSymlink (target link : System.FilePath) : IO Unit := do
  let result ← IO.Process.output { cmd := "ln", args := #["-s", target.toString, link.toString] }
  check (result.exitCode == 0) s!"cannot create fixture symlink: {result.stderr}"

private def analysisAliasChecks (client : Client) (profile : ContextProfile) (code reference : Request) :
    EIO ClientError Unit := do
  let root := client.config.root
  createSymlink (root / "src/lib.rs") (root / "linked.rs")
  createSymlink (root / "src") (root / "linked-src")
  let pathPairs : Array (System.FilePath × System.FilePath) := #[
    ("src/lib.rs", "src/./lib.rs"), ("src/lib.rs", "src/../src/lib.rs"), ("src/lib.rs", "linked.rs"),
    ("src/virtual.rs", "linked-src/virtual.rs")]
  let overlayText := "def sample := 1\n\ncheck sample;\n"
  for (canonical, alias) in pathPairs do
    for (callerPath, profilePath) in #[(canonical, alias), (alias, canonical)] do
      let uri := System.Uri.pathToUri (root / callerPath)
      let profileUri := System.Uri.pathToUri (root / profilePath)
      check (uri != profileUri) "fixture paths did not produce distinct wire URIs"
      let overlay ← client.openDocument uri profile.languageId overlayText
      try
        let aliased := { profile with document := profilePath, collectDiagnostics := fun document cancel? => do
          check (document.uri == overlay.uri && document.version == overlay.version)
            "analysis did not borrow the original document handle"
          profile.collectDiagnostics document cancel? }
        let before ← client.request "test/analysis-state" .null
        check (← failsWith (analyzeWithClient client aliased #[code])
          (fun error => error matches .invalidInput "cannot replace a borrowed document overlay"))
          "analysis accepted different text through an alias"
        check (← failsWith (analyzeWithClient client { aliased with languageId := "other" }
          #[code, reference])
          (fun error => error matches .invalidInput "cannot change a borrowed document language"))
          "analysis accepted a different language through an alias"
        check (← failsWith (client.openDocument profileUri profile.languageId overlayText)
          (fun error => error matches .invalidInput _)) "opening an alias duplicated the original overlay"
        check ((← client.request "test/analysis-state" .null) == before)
          "alias validation sent document traffic"
        let bundle ← analyzeWithClient client aliased #[code, reference]
        let snapshot ← IO.ofExcept <| bundle.find reference
        let canonicalUri := System.Uri.pathToUri (root / canonical)
        check (snapshot.queries[0]?.any (fun query => match query.definitions with
          | .ok targets => targets[0]?.any (·.uri == canonicalUri) && query.localTargets == #[code.key]
          | _ => false)) "analysis lost the canonical definition or its local link through an alias"
        let after ← client.request "test/analysis-state" .null
        let previous ← IO.ofExcept <| before.getObjValAs? (Array Json) "requests"
        let requests ← IO.ofExcept <| after.getObjValAs? (Array Json) "requests"
        let features := requests.extract previous.size requests.size
        check (features.size == 5 && features.all (fun request => request.getObjValD "uri" == toJson uri))
          "feature requests did not preserve the caller's wire URI"
        check (after.getObjValD "openUris" == toJson #[uri] &&
          after.getObjValD "documentEvents" == before.getObjValD "documentEvents")
          "analysis changed the server overlay lifecycle through an alias"
        checkAnalysisOverlay overlay overlayText
      finally
        overlay.close

private def documentIdentityChecks (client : Client) (uri alias : String) : EIO ClientError Unit := do
  let path := (uri.drop "file://".length).copy
  let distinctUris := #[s!"file://rosetta.invalid{path}", s!"{uri}?view=preview", s!"{uri}#fragment",
    s!"untitled:{path}"]
  client.withDocument uri "test" "local overlay" fun localDocument => do
    let documents ← distinctUris.mapM fun distinct => client.openDocument distinct "test" distinct
    let state ← client.request "test/analysis-state" .null
    check (state.getObjValD "openUris" == toJson (#[uri] ++ distinctUris))
      "distinct document URIs did not open independently"
    for document in documents do
      checkAnalysisOverlay document document.uri
      document.close
    checkAnalysisOverlay localDocument "local overlay"
  let start ← IO.Promise.new (α := Unit)
  let openWhenReady (path : String) : EIO ClientError OwnedDocument := do
    IO.wait start.result!
    client.openDocument path "test" "competing overlay"
  let first ← EIO.asTask (openWhenReady uri) .dedicated
  let second ← EIO.asTask (openWhenReady alias) .dedicated
  start.resolve ()
  let winner ← match ← IO.wait first, ← IO.wait second with
    | .ok document, .error (.invalidInput _) | .error (.invalidInput _), .ok document => pure document
    | _, _ => throw <| ClientError.invalidInput "competing alias opens did not admit exactly one document"
  let state ← client.request "test/analysis-state" .null
  check (state.getObjValD "openUris" == toJson #[winner.uri])
    "competing alias opens sent multiple documents to the server"
  checkAnalysisOverlay winner "competing overlay"
  winner.close

private def danglingDocumentChecks (config : ClientConfig) : EIO ClientError Unit := do
  let root := config.root
  let target := root / "src/dangling-target.rs"
  let link := root / "dangling.rs"
  let directory := root / "src/missing-directory"
  let directoryLink := root / "dangling-directory"
  createSymlink target link
  createSymlink directory directoryLink
  Client.withSession { config with initializationOptions := Json.mkObj [("mode", "scope")] } fun client => do
    let traffic := client.request "test/workspace-traffic" .null
    let unresolved (uri : String) : EIO ClientError Unit := do
      let rejected := fun error : ClientError => match error with
        | .invalidInput message => message.startsWith "cannot resolve symbolic link in local document path"
        | _ => false
      check (← failsWith (client.openDocument uri "test" "different text") rejected)
        "a dangling symlink admitted a caller overlay"
      check (← failsWith (client.withDocument uri "test" "different text" fun _ => pure ()) rejected)
        "a dangling symlink admitted a scoped overlay"
      check ((← client.document? uri).isNone) "an unresolved alias found a document"
    let checkDiagnostics (document : Document) : EIO ClientError Unit := do
      let some snapshot ← document.diagnostics | throw <| ClientError.protocol "missing virtual file diagnostics"
      check (snapshot.version? == some document.version &&
        snapshot.diagnostics.map (·.message) == #["current canonical warning"])
        "the virtual file lost the current versioned diagnostics"
    let overlayText := "virtual overlay"
    for (path, alias) in #[(target, link), (directory / "child.rs", directoryLink / "child.rs")] do
      let uri := System.Uri.pathToUri path
      let aliasUri := System.Uri.pathToUri alias
      let before ← traffic
      unresolved aliasUri
      check ((← traffic) == before) "an unresolved alias sent didOpen before its virtual target"
      let owner ← client.openDocument uri "test" overlayText
      try
        let opened ← traffic
        unresolved aliasUri
        check ((← traffic) == opened) "an unresolved alias sent another didOpen for the open virtual target"
        checkAnalysisOverlay owner overlayText
        checkDiagnostics owner
      finally
        owner.close
    -- A wire URI admitted while its link resolved remains usable after the target disappears.
    IO.FS.writeFile target "on disk"
    let uri := System.Uri.pathToUri target
    let aliasUri := System.Uri.pathToUri link
    let impossibleChild := System.Uri.pathToUri (target / "child.rs")
    check (← failsWith (client.openDocument impossibleChild "test" overlayText)
      (fun error => error matches .io (.inappropriateType ..)))
      "a filesystem error was treated as a missing virtual suffix"
    check ((← client.document? impossibleChild).isNone) "an invalid path found a document"
    let owner ← client.openDocument aliasUri "test" overlayText
    try
      let opened ← traffic
      IO.FS.removeFile target
      for requestedUri in #[uri, aliasUri] do
        client.withDocument requestedUri "test" overlayText fun borrowed =>
          check (borrowed.uri == owner.uri && borrowed.version == owner.version)
            "the admitted symbolic link lost its existing overlay after target removal"
        check (← failsWith (client.withDocument requestedUri "test" "different text" fun _ => pure ())
          (fun error => error matches .invalidInput "cannot replace a borrowed document overlay"))
          "the admitted symbolic link accepted conflicting text after target removal"
      check ((← traffic) == opened) "borrowing an admitted link sent another didOpen after target removal"
      checkDiagnostics owner
    finally
      owner.close

private def preCancelledSemanticTokensChecks (profile : ContextProfile) (uri : String) :
    EIO ClientError Unit :=
  Client.withSession profile.client fun client => do
    let document ← client.openDocument uri profile.languageId "test"
    check (← failsWith document.semanticTokens (fun error => error matches .unsupported _))
      "fixture unexpectedly advertised semantic tokens"
    let cancel ← Std.CancellationToken.new
    cancel.cancel
    let isCancelled := fun error => error matches ClientError.cancelled "textDocument/semanticTokens"
    check (← failsWith (document.semanticTokens (cancel? := some cancel)) isCancelled)
      "pre-cancelled semantic tokens checked capability support"
    document.close
    check (← failsWith (document.semanticTokens (cancel? := some cancel)) isCancelled)
      "pre-cancelled semantic tokens checked a stale document"
    check (← failsWith document.semanticTokens (fun error => error matches .staleDocument ..))
      "fixture document remained open after close"
    let state ← client.request "test/analysis-state" .null
    check (state.getObjValD "requests" == .arr #[] &&
      (← client.request "test/cancellations" .null) == .arr #[])
      "pre-cancelled semantic tokens sent request or cancellation traffic"

private def lastAnalysisRequestId (state : Json) (method : String) : EIO ClientError Json := do
  let requests ← IO.ofExcept <| state.getObjValAs? (Array Json) "requests"
  let some request := requests.back? | throw <| ClientError.protocol s!"missing analysis request {method}"
  check (request.getObjValD "method" == toJson method) s!"unexpected analysis request before {method}"
  IO.ofExcept <| request.getObjVal? "id"

private def analysisInterruptionChecks (profile : ContextProfile) (code : Request) (ownerUri : String) :
    EIO ClientError Unit := do
  for (tokenMode, method) in #[("hold-tokens", "textDocument/semanticTokens/full"),
      ("hold-hover", "textDocument/hover"), ("hold-definition", "textDocument/definition")] do
    let held := { profile with
      client := { profile.client with
        initializationOptions := Json.mkObj [("mode", "analysis"), ("tokenMode", toJson tokenMode)] } }
    Client.withSession held.client fun client => do
      let overlayText := code.text ++ "\n"
      let overlay ← client.openDocument ownerUri profile.languageId overlayText
      let cancel ← Std.CancellationToken.new
      let task ← EIO.asTask (analyzeWithClient client held #[code] { cancel? := some cancel }) .dedicated
      awaitNotification client "test/analysis-held"
      let before ← client.request "test/analysis-state" .null
      let cancelledId ← lastAnalysisRequestId before method
      let profileUri := System.Uri.pathToUri (client.config.root / profile.document)
      client.withDocument profileUri profile.languageId overlayText fun document =>
        check (document.uri == ownerUri && document.version == overlay.version)
          "overlapping scopes did not borrow the same alias handle"
      check ((← client.request "test/analysis-state" .null) == before)
        "overlapping scopes changed the active server overlay"
      cancel.cancel
      check (match ← IO.wait task with | .error (.cancelled actual) => actual == method | _ => false)
        s!"analysis did not propagate cancellation to {method}"
      let after ← client.request "test/analysis-state" .null
      check (after.getObjValD "requests" == before.getObjValD "requests")
        "analysis sent more feature requests after cancellation"
      check ((← client.request "test/cancellations" .null) == .arr #[cancelledId])
        s!"server did not receive cancellation for the active {method} request"
      checkAnalysisOverlay overlay overlayText
      overlay.close
      let started ← IO.monoMsNow
      check (← failsWith (analyzeWithClient client held #[code] { requestTimeoutMs? := some 500 })
        (fun error => match error with | .deadline actual => actual == method | _ => false))
        s!"analysis deadline did not reach {method}"
      check ((← IO.monoMsNow) - started < 3000) s!"{method} used the session timeout instead of the override"
      let expired ← client.request "test/analysis-state" .null
      let expiredId ← lastAnalysisRequestId expired method
      check (expired.getObjValD "openUris" == toJson (#[] : Array String)) "analysis deadline leaked its document"
      check (expiredId != cancelledId && (← client.request "test/cancellations" .null) == .arr #[cancelledId, expiredId])
        s!"server did not receive cancellation after the {method} deadline"
  let delayed := { profile with
    client := { profile.client with
      initializationOptions := Json.mkObj [("mode", "analysis"), ("tokenMode", "delay-features")] } }
  let bundle ← analyze #[delayed] #[code] { requestTimeoutMs? := some 650 }
  check (bundle.requests[0]?.any (·.queries.size == 1)) "analysis applied the per-request deadlines to the entire run"

private def analysisDiagnosticInterruptionCheck (profile : ContextProfile) (code : Request) :
    EIO ClientError Unit := do
  let method := "textDocument/diagnostic"
  let options := profile.client.initializationOptions.mergeObj (Json.mkObj [("holdFirstDiagnostic", toJson true)])
  let profile := { profile with
    client := { profile.client with initializationOptions := options }
    collectDiagnostics := fun document cancel? => do
      let params := Json.mkObj [("textDocument", Json.mkObj [("uri", toJson document.uri)])]
      let result ← document.request method params (cancel? := cancel?)
      let diagnostics ← IO.ofExcept <| result.getObjValAs? (Array Lsp.Diagnostic) "items"
      return { coverage := "pull", diagnostics } }
  Client.withSession profile.client fun client => do
    let cancel ← Std.CancellationToken.new
    let task ← EIO.asTask (analyzeWithClient client profile #[code] { cancel? := some cancel }) .dedicated
    awaitNotification client "test/analysis-held"
    let before ← client.request "test/analysis-state" .null
    let cancelledId ← lastAnalysisRequestId before method
    cancel.cancel
    check (match ← IO.wait task with | .error (.cancelled actual) => actual == method | _ => false)
      "analysis did not propagate cancellation to the diagnostic request"
    let after ← client.request "test/analysis-state" .null
    check (after.getObjValD "requests" == before.getObjValD "requests" &&
      after.getObjValD "openUris" == .arr #[]) "cancelled diagnostics admitted feature work or leaked its document"
    check ((← client.request "test/cancellations" .null) == .arr #[cancelledId])
      "server did not receive cancellation for the diagnostic request"
    let retried ← analyzeWithClient client profile #[code]
    check (retried.contexts[0]?.any (·.diagnosticCoverage == "pull") &&
      retried.requests[0]?.any (·.queries.size == 1)) "the reused session did not complete analysis"
    let state ← client.request "test/analysis-state" .null
    check (state.getObjValD "pid" == before.getObjValD "pid" &&
      state.getObjValD "openUris" == .arr #[]) "retry restarted the session or leaked its document"

private def analysisChecks (server : String) : EIO ClientError Unit := do
  let server ← IO.FS.realPath server
  IO.FS.withTempDir fun temporary => do
    let root ← IO.FS.realPath temporary
    IO.FS.createDirAll (root / "src")
    let file := root / "src/lib.rs"
    IO.FS.writeFile file "on disk"
    let uri := (System.Uri.pathToUri file : String)
    let code : Request := {
      key := "body", context := "demo", kind := .code, text := "def sample := 1\n"
      origins := #[⟨"authored", 10, 2⟩, ⟨"authored", 11, 0⟩] }
    let reference : Request := {
      key := "ref", context := "demo", kind := .reference, text := "sample"
      origins := #[⟨"authored", 20, 5⟩] }
    let profile : ContextProfile := {
      key := "demo", languageId := "test"
      client := {
        command := "python3", args := #[server.toString], root
        initializationOptions := Json.mkObj [("mode", "analysis")] }
      document := "src/lib.rs", referencePrefix := "check ", referenceSuffix := ";"
      provenance := Json.mkObj [("revision", "fixture")]
      collectDiagnostics := fun document _ => do
        check (document.uri == uri) "relative document was not resolved against the client root"
        return { coverage := "fixture", completion := toJson true, diagnostics := #[
          { range := range 0 4 0 10, message := "source warning" },
          { range := range 2 0 2 5, message := "generated warning" },
          { range := range 0 10 2 8, message := "mixed warning" },
          { range := range 2 8 2 12, message := "reference warning" }] } }
    let bundle ← analyze #[profile] #[code, reference, code]
    check (bundle.requests.size == 2 && bundle.contexts.size == 1) "analysis did not deduplicate requests"
    let body ← IO.ofExcept <| bundle.find code
    let ref ← IO.ofExcept <| bundle.find reference
    check (match body.tokens with | .ok spans => spans.size == 2 | _ => false)
      "successful semantic token projection was lost"
    check (body.queries.size == 1 &&
      body.diagnostics.map (fun diagnostic => (diagnostic.message, diagnostic.range)) ==
        #[("source warning", range 0 4 0 10), ("mixed warning", range 0 10 1 0)])
      "source query or diagnostic projection failed"
    check (ref.diagnostics.map (fun diagnostic => (diagnostic.message, diagnostic.range)) ==
      #[("mixed warning", range 0 0 0 2), ("reference warning", range 0 2 0 6)])
      "diagnostics did not keep the reference range and order"
    check (match ref.tokens with | .ok spans => spans.map (·.range) == #[range 0 0 0 6] | _ => false)
      "semantic coloring included the generated reference prefix"
    check (ref.queries[0]?.any fun query => match query.hover with
      | .ok (some hover) => hover.text == "mock hover sample"
      | _ => false) "reference hover was lost"
    check (ref.queries[0]?.any (·.localTargets == #["body"])) "reference did not link to the authored definition"
    check (bundle.contexts[0]?.any (fun observation =>
      observation.generatedDiagnostics.map (·.message) == #["generated warning", "mixed warning"] &&
      observation.provenance == profile.provenance && observation.completion == toJson true))
      "generated diagnostic or context evidence was lost"
    let replay ← IO.ofExcept <| (fromJson? (toJson bundle) : Except String Bundle)
    IO.ofExcept replay.validate
    check (toJson replay == toJson bundle) "successful analysis did not survive JSON replay"
    let withoutDiagnostics := { profile with
      collectDiagnostics := fun _ _ => pure { coverage := "none", diagnostics := #[] } }
    Client.withSession profile.client fun client => do
      let before ← client.request "test/analysis-state" .null
      let again ← analyzeWithClient client profile #[code, reference]
      check (toJson again == toJson bundle) "session analysis changed the snapshot"
      let absolute := { profile with document := file, client := { profile.client with root := root / "src" / ".." } }
      let canonical ← analyzeWithClient client absolute #[code, reference]
      check (toJson canonical == toJson bundle) "canonical root or absolute document changed the snapshot"
      let rootLink := root / "root-link"
      createSymlink root rootLink
      let linked := { profile with client := { profile.client with root := rootLink } }
      let fromLinkedRoot ← analyzeWithClient client linked #[code, reference]
      check (toJson fromLinkedRoot == toJson bundle) "symlinked profile root changed the snapshot"
      let state ← client.request "test/analysis-state" .null
      check (state.getObjValD "pid" == before.getObjValD "pid" &&
        state.getObjValD "openUris" == toJson (#[] : Array String)) "analysis restarted the session or leaked its document"
      let overlayText := "def sample := 1\n\ncheck sample;\n"
      let overlay ← client.openDocument uri "test" overlayText
      discard <| analyzeWithClient client profile #[code, reference]
      checkAnalysisOverlay overlay overlayText
      let failing := { profile with collectDiagnostics := fun document _ => do
        check (document.version == overlay.version) "analysis changed the caller overlay version"
        throw <| ClientError.invalidInput "fixture diagnostics failed" }
      check (← failsWith (analyzeWithClient client failing #[code, reference])
        (fun error => error matches .invalidInput "fixture diagnostics failed"))
        "analysis lost the caller's diagnostic error"
      checkAnalysisOverlay overlay overlayText
      check (← failsWith (analyzeWithClient client { profile with languageId := "other" } #[code, reference])
        (fun error => error matches .invalidInput "cannot change a borrowed document language"))
        "analysis accepted a caller overlay with a different language"
      check (← failsWith (analyzeWithClient client profile #[code])
        (fun error => error matches .invalidInput "cannot replace a borrowed document overlay"))
        "analysis accepted a caller overlay with different text"
      checkAnalysisOverlay overlay overlayText
      overlay.close
      let beforeMismatch ← client.request "test/analysis-state" .null
      check (← failsWith (analyzeWithClient client { profile with client := { profile.client with timeoutMs := 1 } } #[code])
        (fun error => error matches .invalidInput _)) "analysis accepted a mismatched session configuration"
      let missingRoot := { profile with client := { profile.client with root := root / "missing" } }
      check (← failsWith (analyzeWithClient client missingRoot #[code])
        (fun error => error matches .invalidInput _)) "missing profile root did not produce invalid input"
      check (← failsWith (analyzeWithClient client profile #[{ code with context := "other" }])
        (fun error => error matches .invalidInput _)) "session analysis accepted a different context"
      check ((← client.request "test/analysis-state" .null) == beforeMismatch)
        "analysis sent document traffic before validating the session or context"
      analysisAliasChecks client withoutDiagnostics code reference
      documentIdentityChecks client uri (System.Uri.pathToUri (root / "src/./lib.rs"))
      danglingDocumentChecks client.config
      analysisBudgetChecks client withoutDiagnostics code reference
      let cancel ← Std.CancellationToken.new
      let cancelling := { withoutDiagnostics with collectDiagnostics := fun _ cancel? => do
        if let some token := cancel? then token.cancel
        return { coverage := "cancel", diagnostics := #[] } }
      let previous ← client.request "test/analysis-state" .null
      check (← failsWith (analyzeWithClient client cancelling #[code] { cancel? := some cancel })
        (fun error => error matches .cancelled "analysis")) "analysis ignored cancellation after the profile callback"
      let current ← client.request "test/analysis-state" .null
      check (current.getObjValD "requests" == previous.getObjValD "requests" &&
        current.getObjValD "openUris" == toJson (#[] : Array String))
        "cancelled callback admitted feature work or leaked its document"
      check ((← client.request "test/cancellations" .null) == .arr #[])
        "cancelling between feature requests sent a cancellation to the server"
    let noTokens := { profile with
      client := { profile.client with
        initializationOptions := Json.mkObj [("mode", "analysis"), ("tokenMode", "none")] } }
    let uncolored ← analyze #[noTokens] #[code, reference]
    check (uncolored.requests[0]?.any (fun snapshot => snapshot.queries.isEmpty &&
        (snapshot.tokens matches .unsupported)) &&
      uncolored.requests[1]?.any (·.queries[0]?.any (·.localTargets == #["body"])))
      "unsupported coloring suppressed an explicit reference"
    preCancelledSemanticTokensChecks noTokens uri
    let unavailable := { profile with client := { profile.client with command := "/missing-server" } }
    let cancel ← Std.CancellationToken.new
    cancel.cancel
    check (← failsWith (analyze #[unavailable] #[code] { cancel? := some cancel })
      (fun error => error matches .cancelled "analysis")) "pre-cancelled analysis started a server"
    let invalidOffset := { unavailable with referenceQueryOffset := fun _ => 1 }
    check (← failsWith (analyze #[invalidOffset] #[{ reference with text := "😀sample" }])
      (fun error => error matches .invalidInput _)) "analysis started a server for a split UTF-8 offset"
    let later := { profile with key := "later" }
    let laterReference := { reference with context := later.key }
    check (← failsWith (analyze #[unavailable] #[code, laterReference])
      (fun error => error matches .invalidInput "expected one profile for context later"))
      "analysis started a server before checking later context membership"
    check (← failsWith (analyze #[unavailable, later, later] #[code, laterReference])
      (fun error => error matches .invalidInput "expected one profile for context later"))
      "analysis started a server before checking later context uniqueness"
    check (← failsWith (analyze #[unavailable, later] #[code, { laterReference with origins := #[] }])
      (fun error => error matches .invalidInput "invalid authored line map: ref"))
      "analysis started a server before checking later authored origins"
    let later := { later with referenceQueryOffset := fun _ => 1 }
    check (← failsWith (analyze #[unavailable, later] #[code, { laterReference with text := "😀sample" }])
      (fun error => error matches .invalidInput "reference query offset is inside a UTF-8 character"))
      "analysis started a server before checking later reference offsets"
    -- A CRLF may span the boundary between generated and authored text.
    for (wrapper, input, offset) in #[("", "x\r\ny", 2), ("\r", "\ny", 0)] do
      let profile := { later with referencePrefix := wrapper, referenceQueryOffset := fun _ => offset }
      let reference := { laterReference with text := input, origins := #[⟨"doc", 0, 0⟩, ⟨"doc", 1, 0⟩] }
      check (← failsWith (analyze #[unavailable, profile] #[code, reference])
        (fun error => error matches .invalidInput _))
        "analysis started a server for a query position inside a CRLF"
    analysisInterruptionChecks withoutDiagnostics code (System.Uri.pathToUri (root / "src/./lib.rs"))
    analysisDiagnosticInterruptionCheck profile code
    check ((← IO.FS.readFile file) == "on disk") "analysis changed the project file"
  IO.println "Analysis projection, replay, root paths, budgets, cancellation and session reuse passed"

def main (args : List String) : IO Unit := do
  match args with
  | ["mock", server] =>
    pureChecks
    bundleChecks
    (localDefinitionChecks server *> mockChecks server *> scopeChecks server *>
      analysisFailureChecks server *> analysisChecks server *> workspaceChecks server).toIO (IO.userError ∘ toString)
  | ["probe", language, command, root] => (probe language command root).toIO (IO.userError ∘ toString)
  | _ => throw <| IO.userError "usage: rosetta-spike mock SERVER.py | probe LANGUAGE COMMAND ROOT"
