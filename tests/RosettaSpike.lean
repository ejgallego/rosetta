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
    collectDiagnostics := fun _ => pure { coverage := "mock", diagnostics := #[
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
      collectDiagnostics := fun _ => pure { coverage := "mock", diagnostics := #[] } }
    let request : Request := {
      key := "body", context := "demo", kind := .code
      text := "x", origins := #[⟨"authored", 0, 0⟩] }
    let result ← (analyze #[profile] #[request]).toBaseIO
    check (match mode, result with
      | "server-error", .ok bundle => bundle.requests[0]?.any (fun snapshot => snapshot.tokens matches .failed _)
      | "disconnect", .error .disconnected => true
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
        | .error (.documentCleanup uri (.staleDocument _ expected actual?)
            (some (.staleDocument ..))) =>
          uri == sourceUri && expected == overlay.version && actual? == some edited.version
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
      let doubleFailure ← (client.withDocument targetUri "test-language" overlayText fun document => do
        document.close
        throw (α := Unit) <| ClientError.invalidInput "action failed").toBaseIO
      assertCheck (match doubleFailure with
        | .error (.documentCleanup _ (.staleDocument ..) (some (.invalidInput "action failed"))) => true
        | _ => false) "document release erased the primary error"
      for edit in #[false, true] do
        for failAction in #[false, true] do
          let owner ← client.openDocument sourceUri "test-language" overlayText
          let result ← (client.withDocument sourceUri "test-language" overlayText fun borrowed => do
            if edit then discard <| borrowed.change "changed overlay" else borrowed.close
            if failAction then throw (α := Unit) <| ClientError.invalidInput "borrowed action failed"
            ).toBaseIO
          let current? ← client.document? sourceUri
          assertCheck (match result with
            | .error (.documentCleanup uri (.staleDocument _ expected actual?) original?) =>
              uri == sourceUri && expected == owner.version && actual? == current?.map (·.version) &&
              (if failAction then original? matches some (.invalidInput "borrowed action failed")
               else original?.isNone)
            | _ => false) "borrowed release lost version invalidation or the primary error"
          if edit then
            let some current := current? | throw <| ClientError.invalidInput "borrowed release closed owner's overlay"
            assertCheck ((← current.text) == "changed overlay") "borrowed release changed owner's overlay"
            current.close
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
        let overlay ← client.openDocument uri "test-language" symbolText
        let source ← readSource
        assertCheck (source.location == virtualLocation && source.pieces.size == 1 &&
          source.pieces[0]!.text == "MyType measure();" &&
          source.pieces[0]!.tokenType.isEmpty && source.pieces[0]!.modifiers.isEmpty)
          "the source helper lost the plain text of the virtual overlay"
        assertCheck ((← overlay.text) == symbolText) "reading a virtual symbol closed its overlay"
        overlay.close
        let traffic ← client.request "test/workspace-traffic" .null
        assertCheck (traffic == Json.mkObj [("documentOpens", toJson 1), ("definitionRequests", toJson 0)])
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
      client.withDocument sourceUri "test-language" mixedText fun _ => do
        let excerpt ← client.sourceExcerpt "test-language" clientLocation
        assertCheck (excerpt.location == clientLocation &&
          String.join (excerpt.pieces.toList.map HighlightedPiece.text) == "\r😀tail")
          "clamping a server location lost the original range or changed the exact source slice"
        for bad in #[range 0 4 0 26, range 0 0 3 0, range 0 100 0 99] do
          assertCheck (← failsWith (client.sourceExcerpt "test-language" { clientLocation with range := bad })
            (fun error => error matches .protocol _)) "invalid server range reached the token request"
    for failureMode in #["server-error", "disconnect", "protocol", "deadline"] do
      Client.withSession { workspaceConfig sourceUri [("tokenMode", toJson failureMode)] with
          timeoutMs := 1000 } fun client => do
        let result ← (client.symbolSource "test-language" "measure" container).toBaseIO
        let detail := match result with | .ok _ => "success" | .error error => toString error
        assertCheck (match result with
          | .error error =>
            match failureMode, error with
            | "server-error", .server (-32601) _ _ => true
            | "disconnect", .documentCleanup _ .disconnected (some .disconnected) => true
            | "protocol", .protocol _ => true
            | "deadline", .deadline _ => true
            | _, _ => false
          | .ok _ => false) s!"symbol source lost the {failureMode} token error: {detail}"
    IO.println "Virtual overlays, semantic token negotiation, and typed failures passed"

def main (args : List String) : IO Unit := do
  match args with
  | ["mock", server] =>
    pureChecks
    bundleChecks
    (localDefinitionChecks server *> mockChecks server *> analysisFailureChecks server *>
      workspaceChecks server).toIO (IO.userError ∘ toString)
  | ["probe", language, command, root] => (probe language command root).toIO (IO.userError ∘ toString)
  | _ => throw <| IO.userError "usage: rosetta-spike mock SERVER.py | probe LANGUAGE COMMAND ROOT"
