/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import RosettaProverProfiles
open Lean Rosetta Rosetta.ProverProfiles

private def check (condition : Bool) (message : String) : EIO ClientError Unit :=
  unless condition do throw <| ClientError.invalidInput message

private def request (context key text : String) (kind : RequestKind := .code) : Request := {
  context, key, text, kind
  origins := (text.splitOn "\n" |>.toArray).mapIdx fun line _ => ⟨"authored://real-acceptance", line, 0⟩ }

private def rustOptions : Json := Json.mkObj [
  ("checkOnSave", toJson true), ("diagnostics", Json.mkObj [("enable", toJson false)])]

private def coqOptions : Json := Json.mkObj [
  ("check_only_on_request", toJson true), ("eager_diagnostics", toJson true),
  ("send_diags", toJson false), ("max_errors", toJson (0 : Nat))]

private def wireEntries (trace : System.FilePath) (offset : Nat)
    (since : Json → Bool := fun _ => true) : IO (List Json) := do
  let contents ← IO.FS.readFile trace
  let tail := String.Pos.Raw.extract contents ⟨offset⟩ ⟨contents.utf8ByteSize⟩
  let entries := (tail.splitOn "\n").filterMap fun line => (Json.parse line).toOption
  return entries.dropWhile fun entry => !since entry

private def awaitWire (trace : System.FilePath) (offset : Nat) (accept : Json → Bool)
    (ready : BaseIO Bool := pure true) (since : Json → Bool := fun _ => true) : EIO ClientError Json := do
  let deadline := (← IO.monoMsNow) + 10000
  repeat
    if ← ready then
      if let some entry := (← wireEntries trace offset since).find? accept then return entry
    if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline "real transport capture"
    IO.sleep 2
  throw <| ClientError.protocol "unreachable wire wait"

private def wireMethod (direction method : String) (entry : Json) : Bool :=
  entry.getObjValD "direction" == toJson direction && entry.getObjValD "method" == toJson method

private def cancelAfter (method : String) (action : Std.CancellationToken → EIO ClientError α)
    (observe : EIO ClientError β) : EIO ClientError β := do
  let cancel ← Std.CancellationToken.new
  let task ← EIO.asTask (action cancel) .dedicated
  let observed ← observe.toBaseIO
  cancel.cancel
  let outcome ← IO.wait task
  match outcome with
  | .error (.cancelled actual) =>
    let evidence ← EIO.ofExcept observed
    check (actual == method) s!"cancellation observed during {actual}, expected {method}"
    return evidence
  | .error error => throw error
  | .ok _ =>
    discard <| EIO.ofExcept observed
    throw <| .invalidInput s!"{method} completed before cancellation"

private def cancelOnWire (trace : System.FilePath) (method : String)
    (action : Std.CancellationToken → EIO ClientError α) : EIO ClientError Json := do
  let offset := (← IO.FS.readFile trace).utf8ByteSize
  let sent ← cancelAfter method action (awaitWire trace offset (wireMethod "clientToServer" method))
  let id ← EIO.ofExcept <| (sent.getObjValAs? Nat "id").mapError ClientError.protocol
  let cancels := fun entry =>
    wireMethod "clientToServer" "$/cancelRequest" entry && entry.getObjValD "cancelId" == toJson id
  let responds := fun (entry : Json) =>
    entry.getObjValD "direction" == toJson "serverToClient" && !(entry.getObjVal? "method").isOk &&
    entry.getObjValD "id" == toJson id
  let first ← awaitWire trace offset fun entry => cancels entry || responds entry
  check (cancels first) s!"server replied to {method} before cancellation was forwarded"
  return Json.mkObj [("method", toJson method), ("id", toJson id)]

private def diagnostics (bundle : Bundle) : Array Lsp.Diagnostic :=
  (bundle.requests.flatMap (·.diagnostics)) ++ (bundle.contexts.flatMap (·.generatedDiagnostics))

private def checkRocqCompletion (bundle : Bundle) (expected : String) : EIO ClientError Unit := do
  let status := bundle.contexts[0]?.bind fun context =>
    ((context.completion.getObjVal? "completed") >>= (·.getObjValAs? (Array String) "status")).toOption
  check (status == some #[expected]) s!"Rocq completion was {repr status}, expected {expected}"

private def checkValid (client : Client) (rust : Bool) (body reference : Request) (bundle : Bundle) :
    EIO ClientError Unit := do
  let caps ← client.capabilities
  let source ← EIO.ofExcept <| (bundle.find body).mapError ClientError.invalidInput
  if caps.getObjValD "semanticTokensProvider" != .null then
    check (match source.tokens with | .ok spans => !spans.isEmpty | _ => false)
      "advertised semantic coloring produced no spans"
  else check (source.tokens matches .unsupported) "unsupported coloring was misclassified"
  let ref ← EIO.ofExcept <| (bundle.find reference).mapError ClientError.invalidInput
  check (ref.queries[0]?.any fun query =>
    (query.hover matches .ok (some _)) &&
    (match query.definitions with | .ok targets => !targets.isEmpty | _ => false) &&
    query.localTargets == #[body.key]) "explicit reference lost hover or navigation to its definition block"
  check (!(diagnostics bundle).any (·.severity? == some .error)) "valid overlay has errors"
  if rust then
    check (bundle.contexts[0]?.any fun context =>
      let status := context.completion.getObjValD "observedProcessStatus"
      status.getObjValD "quiescent" == toJson true &&
        ((context.completion.getObjValAs? Int "admittedDocumentVersion").toOption.any (· > 0)))
      "Rust diagnostics did not record readiness and the admitted document version"
  else
    checkRocqCompletion bundle "Yes"

private def cancelRocqWait (client : Client) (profile : ContextProfile) (long : Request)
    (trace : System.FilePath) : EIO ClientError Json := do
  let entered ← IO.Promise.new
  let observed := { profile with collectDiagnostics := fun doc cancel? => do
    entered.resolve ()
    profile.collectDiagnostics doc cancel? }
  let offset := (← IO.FS.readFile trace).utf8ByteSize
  let label := "coq-lsp current-version diagnostics"
  let version ← cancelAfter label (fun cancel =>
    analyzeWithClient client observed #[long] { cancel? := some cancel, maxQueriesPerCodeBlock := 0 }) do
    let opened ← awaitWire trace offset (wireMethod "clientToServer" "textDocument/didOpen")
      (IO.hasFinished entered.result?)
    let version ← EIO.ofExcept <| (opened.getObjValAs? Int "version").mapError ClientError.protocol
    discard <| awaitWire trace offset fun entry =>
      wireMethod "serverToClient" "$/coq/fileProgress" entry && entry.getObjValD "version" == toJson version
    pure version
  let opened := fun entry =>
    wireMethod "clientToServer" "textDocument/didOpen" entry && entry.getObjValD "version" == toJson version
  let didClose := wireMethod "clientToServer" "textDocument/didClose"
  discard <| awaitWire trace offset didClose (since := opened)
  let entries ← wireEntries trace offset opened
  check (!entries.any fun entry =>
    wireMethod "clientToServer" "coq/getDocument" entry && entry.getObjValD "version" == toJson version)
    "cancelled Rocq diagnostics wait sent a completion request"
  check (!(entries.takeWhile (fun entry => !didClose entry)).any fun entry =>
    wireMethod "serverToClient" "textDocument/publishDiagnostics" entry && entry.getObjValD "version" == toJson version)
    "Rocq sent terminal diagnostics before cancellation released the overlay"
  return Json.mkObj [("phase", toJson label), ("admittedDocumentVersion", toJson version)]

private def runCases (client : Client) (profile : ContextProfile) (trace : System.FilePath) :
    EIO ClientError Json := do
  let language := profile.key
  let rust := language == "rust"
  let path := client.config.root / profile.document
  let uri := (System.Uri.pathToUri path : String)
  let original ← IO.FS.readFile path
  let body := request language "body" (if rust then "    pub fn step(n: u64) -> u64 { n + 1 }\n"
    else "Definition step (n : nat) : nat := n + 1.\n")
  let call := request language "call" (if rust then
    "}\npub fn example() -> u64 { /* 😀 */ arithmetic::step(2) }\n"
    else "Lemma example : step 1 = 2. Proof. reflexivity. Qed.\n")
  let preamble := if rust then #[request language "module" "pub mod arithmetic {\n"] else #[]
  let reference := request language "reference" (if rust then "arithmetic::step" else "step") .reference
  let code := preamble ++ #[body, call]
  let valid := code.push reference
  let bundle ← analyzeWithClient client profile (valid.push body)
  check (bundle.requests.size == valid.size) "repeated request was not shared"
  checkValid client rust body reference bundle

  let invalid := request language "invalid" (if rust then "fn invalid( {\n"
    else String.join (List.replicate 20 "Check missing_name.\n"))
  let errors ← analyzeWithClient client profile (code.push invalid)
  let authored ← EIO.ofExcept <| (errors.find invalid).mapError ClientError.invalidInput
  check (authored.diagnostics.any (·.severity? == some .error)) "invalid authored fragment has no mapped error"
  let mut extras := #[]
  if !rust then
    checkRocqCompletion errors "Failed"
    check ((diagnostics errors).any (·.message == "Maximum number of errors reached"))
      "Rocq omitted the error limit diagnostic"
    let empty := request language "empty" ""
    let clean ← analyzeWithClient client profile #[empty]
    checkRocqCompletion clean "Yes"
    check ((diagnostics clean).isEmpty) "empty Rocq overlay retained diagnostics"
    extras := extras.push ("empty", toJson clean)
  let restored ← analyzeWithClient client profile valid
  checkValid client rust body reference restored

  let large := if rust then String.join ((List.range 20000).map fun n =>
      "fn f" ++ toString n ++ "() -> u64 { " ++ toString n ++ " }\n")
    else String.join ((List.range 10000).map fun n => s!"Lemma p{n} : 1 = 1. Proof. reflexivity. Qed.\n")
  let long := request language "long" large
  let cancellation ← if rust then
    cancelOnWire trace "textDocument/diagnostic" fun cancel =>
      analyzeWithClient client profile #[long] { cancel? := some cancel, maxQueriesPerCodeBlock := 0 }
  else do
    let cancelledWait ← cancelRocqWait client profile long trace
    extras := extras.push ("cancelledPushWait", cancelledWait)
    check ((← client.document? uri).isNone) "cancelled Rocq diagnostics wait leaked its overlay"
    let recovered ← analyzeWithClient client profile valid
    checkValid client rust body reference recovered
    extras := extras.push ("afterPushWaitCancel", toJson recovered)
    -- Exercise cancellation of the completion RPC before the large overlay reaches terminal diagnostics.
    cancelOnWire trace "coq/getDocument" fun cancel =>
      client.withDocument uri profile.languageId large fun doc =>
        doc.request "coq/getDocument" (Json.mkObj [
          ("textDocument", Json.mkObj [("uri", toJson doc.uri), ("version", toJson doc.version)])])
          (cancel? := some cancel)
  check ((← client.document? uri).isNone) "cancelled overlay leaked"
  let afterCancel ← analyzeWithClient client profile valid
  checkValid client rust body reference afterCancel
  check ((← client.document? uri).isNone) "scoped overlay leaked"
  check ((← IO.FS.readFile path) == original) "analysis changed fixture on disk"
  return Json.mkObj <| [
    ("language", toJson language), ("bundle", toJson bundle), ("invalid", toJson errors),
    ("restored", toJson restored), ("afterCancel", toJson afterCancel), ("cancellation", cancellation)] ++ extras.toList

private def run (language command root wrapper trace : String) : EIO ClientError Unit := do
  let rust := language == "rust"
  check (rust || language == "rocq") "unknown real acceptance language"
  let profile ← (if rust then rustProfile else coqProfile)
    { client := {
        command := "python3", args := #[wrapper, trace, command], root := root, timeoutMs := 60000
        initializationOptions := if rust then rustOptions else coqOptions } }
    language (if rust then "src/lib.rs" else "Probe.v")
  let client ← Client.start profile.client
  let result ← (runCases client profile trace).toBaseIO
  let closed ← client.close
  let cleanup := Json.mkObj [("language", toJson language), ("cleanup", Json.mkObj [
    ("complete", toJson closed.complete), ("reaped", toJson closed.reaped),
    ("workersStopped", toJson closed.workersStopped), ("forced", toJson closed.forced),
    ("errors", toJson closed.errors)])]
  let logged ← (IO.println cleanup.compress).toBaseIO
  if !closed.complete || !result.isOk then
    let stderr ← client.stderr
    unless stderr.isEmpty do
      discard <| (IO.eprintln stderr).toBaseIO
  let report ← EIO.ofExcept <| closed.finish result
  EIO.ofExcept <| logged.mapError ClientError.io
  IO.println report.compress

def main (args : List String) : IO Unit :=
  match args with
  | [language, command, root, wrapper, trace] => (run language command root wrapper trace).toIO (IO.userError ∘ toString)
  | _ => throw <| IO.userError "usage: rosetta-real-provers (rust|rocq) SERVER PROJECT CAPTURE.py TRACE"
