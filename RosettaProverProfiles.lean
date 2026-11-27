/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
module
public import Rosetta
public section
namespace Rosetta.ProverProfiles
open Lean

/-- Caller-owned process/project configuration; profiles add protocol policy only.
The caller must provide an existing project member and a working server environment. -/
structure Config where
  client : ClientConfig
  readinessTimeoutMs : Nat := 60000
  provenance : Json := .null

private def objectOptions (value : Json) (name : String) : EIO ClientError Json := do
  if value == .null then return Json.mkObj []
  unless value.getObj?.isOk do throw <| ClientError.invalidInput s!"{name} must be an object or null"
  return value

private def isSimpleRustPathSegment (segment : String) : Bool :=
  let isStart (character : Char) :=
    character.isAlpha || character == '_' || character.toNat ≥ 128
  match (segment.dropPrefix "r#").toString.toList with
  | [] => false
  | first :: rest => isStart first && rest.all (fun character => isStart character || character.isDigit)

private def rustReferenceOffset (reference : String) : Nat := Id.run do
  let segments := (reference.dropPrefix "::").toString.splitOn "::"
  if segments.all isSimpleRustPathSegment then
    if let some last := segments.getLast? then
      return reference.utf8ByteSize - last.utf8ByteSize
  return 0

/-- The rust-analyzer profile enables native pull diagnostics and disables check-on-save,
build scripts and proc macros in the initialization options. Readiness observes process-wide
quiescence, which may predate the overlay. The diagnostic request uses the admitted document
handle. Simple paths separated by `::` are queried at the final segment, with optional leading
`::` and raw identifiers. Non-ASCII characters are identifier candidates for rust-analyzer to
validate. Other references start at byte zero. Callers can set `referenceQueryOffset` on the
returned profile for other reference syntax. -/
def rustProfile (config : Config) (key : String) (document : System.FilePath) :
    EIO ClientError ContextProfile := do
  let options ← objectOptions config.client.initializationOptions "initializationOptions"
  let cargo ← objectOptions (options.getObjValD "cargo") "cargo"
  let scripts ← objectOptions (cargo.getObjValD "buildScripts") "cargo.buildScripts"
  let procMacro ← objectOptions (options.getObjValD "procMacro") "procMacro"
  let diagnostics ← objectOptions (options.getObjValD "diagnostics") "diagnostics"
  let experimental ← objectOptions config.client.experimentalCapabilities "experimentalCapabilities"
  let client := { config.client with
    initializationOptions := options.mergeObj <| Json.mkObj [
      ("checkOnSave", toJson false),
      ("cargo", cargo.mergeObj <| Json.mkObj [("buildScripts", scripts.mergeObj <| Json.mkObj [("enable", toJson false)])]),
      ("procMacro", procMacro.mergeObj <| Json.mkObj [("enable", toJson false)]),
      ("diagnostics", diagnostics.mergeObj <| Json.mkObj [("enable", toJson true)])]
    experimentalCapabilities := experimental.mergeObj <| Json.mkObj [("serverStatusNotification", toJson true)]
    retainedNotificationMethods := config.client.retainedNotificationMethods.push "experimental/serverStatus" }
  return {
    key, languageId := "rust", client, document
    referencePrefix := "const _: () = { let _ = ", referenceSuffix := "; };"
    referenceQueryOffset := rustReferenceOffset
    provenance := config.provenance
    collectDiagnostics := fun doc cancel? => do
      let status ← doc.client.waitForNotification "experimental/serverStatus" (fun status =>
        (status.getObjValAs? Bool "quiescent").toOption == some true) config.readinessTimeoutMs cancel?
      let params := Json.mkObj [("textDocument", Json.mkObj [("uri", toJson doc.uri)])]
      let report ← doc.request "textDocument/diagnostic" params config.readinessTimeoutMs cancel?
      let diagnostics ← EIO.ofExcept <| (report.getObjValAs? (Array Lsp.Diagnostic) "items").mapError ClientError.protocol
      return {
        coverage := "rust-analyzer native pull diagnostics (cargo check disabled)"
        completion := Json.mkObj [("observedProcessStatus", status),
          ("admittedDocumentVersion", toJson doc.version)]
        diagnostics } }

private def waitForCoqDiagnostics (doc : Document) (timeoutMs : Nat)
    (cancel? : Option Std.CancellationToken) : EIO ClientError DiagnosticSnapshot := do
  let operation := "coq-lsp current-version diagnostics"
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if let some token := cancel? then
      if ← token.isCancelled then throw <| ClientError.cancelled operation
    if let some snapshot ← doc.diagnostics then
      if snapshot.version? == some doc.version then return snapshot
    if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline operation
    IO.sleep 10
  throw <| ClientError.protocol "unreachable diagnostic wait"

/-- The coq-lsp profile enables automatic checking and final push diagnostics.
It waits for the admitted document version's final push before asking for
`coq/getDocument`. coq-lsp 0.2.4 can leave an earlier request pending if checking
ends in `Failed`. The recorded `Yes` or `Failed` status describes completion,
and the diagnostic set reports errors in the checked portion. -/
def coqProfile (config : Config) (key : String) (document : System.FilePath) :
    EIO ClientError ContextProfile := do
  let options ← objectOptions config.client.initializationOptions "initializationOptions"
  let client := { config.client with initializationOptions := options.mergeObj <| Json.mkObj [
    ("check_only_on_request", toJson false),
    ("eager_diagnostics", toJson false),
    ("send_diags", toJson true)] }
  return {
    key, languageId := "coq", client, document
    referencePrefix := "Check ", referenceSuffix := "."
    provenance := config.provenance
    collectDiagnostics := fun doc cancel? => do
      let snapshot ← waitForCoqDiagnostics doc config.readinessTimeoutMs cancel?
      let params := Json.mkObj [("textDocument", Json.mkObj [
        ("uri", toJson doc.uri), ("version", toJson doc.version)])]
      let completion ← doc.request "coq/getDocument" params config.readinessTimeoutMs cancel?
      let status ← EIO.ofExcept <| ((completion.getObjVal? "completed") >>= (·.getObjValAs? (Array String) "status")).mapError ClientError.protocol
      unless status == #["Yes"] || status == #["Failed"] do
        throw <| ClientError.protocol s!"expected terminal coq-lsp completion status: {repr status}"
      return {
        coverage := "coq-lsp final current-version push diagnostics and getDocument", completion
        diagnostics := snapshot.diagnostics } }
end Rosetta.ProverProfiles
