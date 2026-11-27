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

private def profileChecks : EIO ClientError Unit := do
  let options := Json.mkObj [("custom", toJson "preserved"), ("cargo", Json.mkObj [
    ("features", toJson #["custom-feature"]), ("buildScripts", Json.mkObj [
      ("invocationStrategy", toJson "per_workspace"), ("enable", toJson true)])])]
  let config : Config := { client := {
    command := "unused", root := ".", initializationOptions := options
    experimentalCapabilities := Json.mkObj [("customExtension", toJson true)] } }
  let rust ← rustProfile config "rust" "src/lib.rs"
  let actual := rust.client.initializationOptions
  check (actual.getObjValD "custom" == toJson "preserved" &&
    (actual.getObjValD "cargo").getObjValD "features" == toJson #["custom-feature"] &&
    ((actual.getObjValD "cargo").getObjValD "buildScripts").getObjValD "invocationStrategy" == toJson "per_workspace" &&
    ((actual.getObjValD "cargo").getObjValD "buildScripts").getObjValD "enable" == toJson false &&
    rust.client.experimentalCapabilities.getObjValD "customExtension" == toJson true)
    "Rust profile discarded caller configuration or failed to apply its policy"
  let coq ← coqProfile config "coq" "Probe.v"
  check (coq.client.initializationOptions.getObjValD "custom" == toJson "preserved")
    "Coq profile discarded caller configuration"
  let invalid ← (rustProfile { config with client := { config.client with initializationOptions := toJson true } }
    "rust" "src/lib.rs").toBaseIO
  check (invalid matches .error (.invalidInput _)) "malformed profile options lost typed error"

private def awaitWire (trace : System.FilePath) (offset : Nat) (accept : Json → Bool) : EIO ClientError Json := do
  let deadline := (← IO.monoMsNow) + 10000
  repeat
    let contents ← IO.FS.readFile trace
    let tail := String.Pos.Raw.extract contents ⟨offset⟩ ⟨contents.utf8ByteSize⟩
    for line in tail.splitOn "\n" do
      if let .ok entry := Json.parse line then
        if accept entry then return entry
    if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline "real transport capture"
    IO.sleep 2
  throw <| ClientError.protocol "unreachable wire wait"

private def hasErrors (bundle : Bundle) : Bool :=
  bundle.requests.any (fun snapshot => snapshot.diagnostics.any (·.severity? == some .error)) ||
  bundle.contexts.any (fun context => context.generatedDiagnostics.any (·.severity? == some .error))

private def checkValid (client : Client) (rust : Bool) (body reference : Request) (bundle : Bundle) :
    EIO ClientError Unit := do
  let caps ← client.capabilities
  let source ← EIO.ofExcept <| (bundle.find body).mapError ClientError.invalidInput
  if caps.getObjValD "semanticTokensProvider" != .null then
    check (match source.tokens with | .ok spans => !spans.isEmpty | _ => false)
      "advertised semantic coloring produced no spans"
  else check (source.tokens matches .unsupported) "unsupported coloring was misclassified"
  let ref ← EIO.ofExcept <| (bundle.find reference).mapError ClientError.invalidInput
  check (ref.queries[0]?.any (fun query =>
    (query.hover matches .ok (some _)) &&
    (match query.definitions with | .ok targets => !targets.isEmpty | _ => false) && query.localTargets.contains body.key))
    "explicit reference lacks hover, definition, or local target"
  check (!hasErrors bundle) "valid overlay has errors"
  if !rust then
    check (bundle.contexts[0]?.any (fun context =>
      ((context.completion.getObjVal? "completed") >>= (·.getObjValAs? (Array String) "status")).toOption == some #["Yes"]))
      "valid Rocq overlay did not complete"

private def run (language command root wrapper trace : String) : EIO ClientError Unit := do
  let rust := language == "rust"
  let profile ← (if rust then rustProfile else coqProfile)
    { client := { command := "python3", args := #[wrapper, trace, command], root := root, timeoutMs := 60000 } }
    language (if rust then "src/lib.rs" else "Probe.v")
  let body := request language "body" (if rust then
    "pub fn step(n: u64) -> u64 { n + 1 }\npub fn example() -> u64 { /* 😀 */ step(2) }\n"
    else "Definition step (n : nat) : nat := n + 1.\nLemma example : step 1 = 2. Proof. reflexivity. Qed.\n")
  let reference := request language "reference" "step" .reference
  let original ← IO.FS.readFile (profile.client.root / profile.document)
  let client ← Client.start profile.client
  let mut report := Json.null
  try
    let bundle ← analyzeWithClient client profile #[body, reference, body]
    check (bundle.requests.size == 2) "repeated request was not shared"
    checkValid client rust body reference bundle
    let invalid := request language "invalid" (if rust then "fn invalid( {\n" else "Check missing_name.\n")
    let errors ← analyzeWithClient client profile #[body, invalid]
    check ((← EIO.ofExcept <| (errors.find invalid).mapError ClientError.invalidInput).diagnostics.any
      (·.severity? == some .error)) "invalid authored fragment has no mapped error"
    let restored ← analyzeWithClient client profile #[body, reference]
    check (!hasErrors restored) "restored overlay retained stale error"
    -- Wait for the transport recorder to forward the diagnostic request before cancelling.
    let uri := (System.Uri.pathToUri (profile.client.root / profile.document) : String)
    let large := if rust then String.join ((List.range 20000).map fun n => ("fn f" ++ toString n ++ "() -> u64 { " ++ toString n ++ " }\n"))
      else String.join ((List.range 10000).map fun n => s!"Lemma p{n} : 1 = 1. Proof. reflexivity. Qed.\n")
    let cancel ← Std.CancellationToken.new
    let method := if rust then "textDocument/diagnostic" else "coq/getDocument"
    let offset := (← IO.FS.readFile trace).utf8ByteSize
    let task ← EIO.asTask (analyzeWithClient client profile #[request language "long" large]
      { cancel? := some cancel, maxQueriesPerCodeBlock := 0 }) .dedicated
    let sent ← try
        awaitWire trace offset fun entry =>
          (entry.getObjValAs? String "direction").toOption == some "clientToServer" &&
          (entry.getObjValAs? String "method").toOption == some method
      finally cancel.cancel
    let outcome ← IO.wait task
    let id ← EIO.ofExcept <| (sent.getObjValAs? Nat "id").mapError ClientError.protocol
    check (match outcome with | .error (.cancelled actual) => actual == method | _ => false)
      s!"real {language} request did not expose typed cancellation"
    let forwarded ← awaitWire trace offset fun entry =>
      if (entry.getObjValAs? String "direction").toOption == some "serverToClient" then
        (entry.getObjValAs? Nat "id").toOption == some id
      else
        (entry.getObjValAs? String "method").toOption == some "$/cancelRequest" &&
        (entry.getObjValAs? Nat "cancelId").toOption == some id
    check ((forwarded.getObjValAs? Nat "cancelId").toOption == some id)
      s!"real {language} server answered before cancellation was forwarded"
    check ((← client.document? uri).isNone) "cancelled overlay leaked"
    let afterCancel ← analyzeWithClient client profile #[body, reference]
    checkValid client rust body reference afterCancel
    check ((← client.document? uri).isNone) "scoped overlay leaked"
    check ((← IO.FS.readFile (profile.client.root / profile.document)) == original) "analysis changed fixture on disk"
    report := Json.mkObj [("language", toJson language), ("bundle", toJson bundle),
      ("invalid", toJson errors), ("restored", toJson restored), ("afterCancel", toJson afterCancel),
      ("cancellation", Json.mkObj [("method", toJson method), ("id", toJson id)])]
  finally
    let closed ← client.close
    check closed.complete s!"incomplete real process cleanup: {repr closed}"
    IO.println (Json.mkObj [("language", toJson language), ("cleanup", Json.mkObj [
      ("complete", toJson closed.complete), ("reaped", toJson closed.reaped),
      ("workersStopped", toJson closed.workersStopped), ("forced", toJson closed.forced),
      ("errors", toJson closed.errors)])]).compress
  IO.println report.compress

def main (args : List String) : IO Unit :=
  match args with
  | [language, command, root, wrapper, trace] => (profileChecks *> run language command root wrapper trace).toIO (IO.userError ∘ toString)
  | _ => throw <| IO.userError "usage: rosetta-real-provers (rust|rocq) SERVER PROJECT CAPTURE.py TRACE"
