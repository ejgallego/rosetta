/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import VersoRosetta
import Rosetta

open Lean Rosetta Verso Verso.Doc Verso.Genre.Manual VersoRosetta

namespace RosettaWorkflow

-- Compilation records requests; it performs no prover IO.
#docs (Genre.Manual) chapter "Foreign code" :=
:::::::
```lspCode "body" (context := "demo")
def sample := 1
```

The reference {lspRef "reference" "sample" (context := "demo")}[] resolves in the same context.
:::::::

/-- Repeated inclusions share analysis, but each occurrence gets its own HTML ID. -/
def document : Part Genre.Manual := {
  title := #[.text "Rosetta workflow"], titleString := "Rosetta workflow"
  metadata := none, content := #[]
  subParts := #[
    { chapter.toPart with metadata := some { file := some "first", tag := some "first" } },
    { chapter.toPart with
      title := #[.text "Repeated inclusion"], titleString := "Repeated inclusion"
      metadata := some { file := some "second", tag := some "second" } }] }

/-- The mock's pull diagnostic response is its readiness barrier. Real prover
profiles must supply their own completion/diagnostic evidence. -/
def diagnostics (document : Rosetta.Document) (cancel? : Option Std.CancellationToken) :
    EIO ClientError DiagnosticEvidence := do
  let params := Json.mkObj [("textDocument", Json.mkObj [("uri", toJson document.uri)])]
  let report ← document.request "textDocument/diagnostic" params (cancel? := cancel?)
  let diagnostics ← EIO.ofExcept <|
    (report.getObjValAs? (Array Lsp.Diagnostic) "items").mapError ClientError.protocol
  return { coverage := "Mock pull diagnostics for the opened overlay", completion := report, diagnostics }

/-- Explicit IO stage. The temporary fixture file belongs to the profile's
project. Rosetta opens an overlay without rewriting this file. -/
def analyzeDocument (server : System.FilePath) : EIO ClientError Bundle := do
  let server ← IO.FS.realPath server
  IO.FS.withTempDir fun root => do
    let file := root / "Example.mock"
    IO.FS.writeFile file ""
    let profile : ContextProfile := {
      key := "demo", languageId := "mock"
      client := {
        command := "python3", args := #[server.toString], root
        initializationOptions := Json.mkObj [("mode", toJson "analysis")] }
      document := file, referencePrefix := "", referenceSuffix := ""
      provenance := Json.mkObj [("profile", toJson "rosetta-workflow-mock")]
      collectDiagnostics := diagnostics }
    let requests ← EIO.ofExcept <| (collect document).mapError ClientError.invalidInput
    analyze #[profile] requests

def saveBundle (file : System.FilePath) (bundle : Bundle) : IO Unit := do
  IO.ofExcept bundle.validate
  IO.FS.writeFile file (toJson bundle).pretty

def loadBundle (file : System.FilePath) : IO Bundle := do
  let json ← IO.ofExcept <| Json.parse (← IO.FS.readFile file)
  let bundle ← IO.ofExcept <| (fromJson? json : Except String Bundle)
  IO.ofExcept bundle.validate
  return bundle

/-- Attach saved analysis to the compiled document and run Verso's Manual renderer. -/
def render (bundle : Bundle) (output : System.FilePath) : IO UInt32 := do
  let enriched ← IO.ofExcept <| enrich bundle document
  manualMain enriched (options := []) (config := {
    destination := output, emitHtmlSingle := .immediately, emitHtmlMulti := .immediately
    features := {} })

end RosettaWorkflow
