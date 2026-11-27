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

private def params (document : Document) : Json := Json.mkObj [
  ("textDocument", Json.mkObj [("uri", toJson document.uri), ("version", toJson document.version)])]

/-- Rust profile for rust-analyzer with native pull diagnostics. Check-on-save,
build scripts and proc macros are disabled for these documentation overlays.
Readiness observes the latest process-wide quiescent status (which may predate
the overlay); diagnostics are then pulled for the admitted document version. This covers rust-analyzer's native diagnostics, not cargo check. -/
def rustProfile (config : Config) (key : String) (document : System.FilePath) :
    EIO ClientError ContextProfile := do
  let options ← objectOptions config.client.initializationOptions "initializationOptions"
  let cargo ← objectOptions (options.getObjValD "cargo") "cargo"
  let scripts ← objectOptions (cargo.getObjValD "buildScripts") "cargo.buildScripts"
  let procMacro ← objectOptions (options.getObjValD "procMacro") "procMacro"
  let experimental ← objectOptions config.client.experimentalCapabilities "experimentalCapabilities"
  let client := { config.client with
    initializationOptions := options.mergeObj <| Json.mkObj [
      ("checkOnSave", toJson false),
      ("cargo", cargo.mergeObj <| Json.mkObj [("buildScripts", scripts.mergeObj <| Json.mkObj [("enable", toJson false)])]),
      ("procMacro", procMacro.mergeObj <| Json.mkObj [("enable", toJson false)])]
    experimentalCapabilities := experimental.mergeObj <| Json.mkObj [("serverStatusNotification", toJson true)]
    retainedNotificationMethods := config.client.retainedNotificationMethods.push "experimental/serverStatus" }
  return {
    key, languageId := "rust", client, document
    referencePrefix := "const _: () = { let _ = ", referenceSuffix := "; };"
    provenance := config.provenance
    collectDiagnostics := fun doc cancel? => do
      doc.client.waitForNotification "experimental/serverStatus" (fun status =>
        (status.getObjValAs? Bool "quiescent").toOption == some true) config.readinessTimeoutMs cancel?
      let status ← doc.client.latestNotification "experimental/serverStatus"
      let report ← doc.request "textDocument/diagnostic" (params doc) config.readinessTimeoutMs cancel?
      let diagnostics ← EIO.ofExcept <| (report.getObjValAs? (Array Lsp.Diagnostic) "items").mapError ClientError.protocol
      return {
        coverage := "rust-analyzer native pull diagnostics (cargo check disabled)"
        completion := Json.mkObj [("observedProcessStatus", toJson status),
          ("diagnosticDocumentVersion", toJson doc.version)]
        diagnostics } }

/-- coq-lsp profile. getDocument checks the exact document version through its
end before current-version push diagnostics are read. Stopped/failed completion
is recorded as evidence; it is never relabeled as a successful proof check. -/
def coqProfile (config : Config) (key : String) (document : System.FilePath) :
    EIO ClientError ContextProfile := do
  let options ← objectOptions config.client.initializationOptions "initializationOptions"
  let client := { config.client with initializationOptions := options.mergeObj <| Json.mkObj [
    ("check_only_on_request", toJson false)] }
  return {
    key, languageId := "coq", client, document
    referencePrefix := "Check ", referenceSuffix := "."
    provenance := config.provenance
    collectDiagnostics := fun doc cancel? => do
      let completion ← doc.request "coq/getDocument" (params doc) config.readinessTimeoutMs cancel?
      let status ← EIO.ofExcept <| ((completion.getObjVal? "completed") >>= (·.getObjValAs? (Array String) "status")).mapError ClientError.protocol
      unless #[#["Yes"], #["Stopped"], #["Failed"]].contains status do
        throw <| ClientError.protocol s!"unknown coq-lsp completion status: {repr status}"
      let deadline := (← IO.monoMsNow) + config.readinessTimeoutMs
      repeat
        if let some token := cancel? then
          if ← token.isCancelled then throw <| ClientError.cancelled "coq-lsp current-version diagnostics"
        if let some snapshot ← doc.diagnostics then
          if snapshot.version? == some doc.version then
            return {
              coverage := "coq-lsp getDocument and current-version push diagnostics", completion
              diagnostics := snapshot.diagnostics }
        if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline "coq-lsp current-version diagnostics"
        IO.sleep 10
      throw <| ClientError.protocol "unreachable diagnostic wait" }
end Rosetta.ProverProfiles
