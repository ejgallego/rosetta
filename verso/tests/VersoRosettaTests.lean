/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import VersoRosetta
import RosettaWorkflow

open Verso Verso.Doc Verso.Genre.Manual VersoRosetta

namespace VersoRosettaTests

#docs (Genre.Manual) document "Inert foreign-language requests" :=
:::::::
```lspCode "body" (context := "demo")
foreign code
```

A named reference {lspRef "reference" "foreign" (context := "demo")}[] shares its context.
:::::::

#guard (collect document.toPart).toOption.any fun requests =>
  requests.size == 2 && requests[0]!.kind == .code && requests[1]!.kind == .reference &&
    requests.all (·.context == "demo") && requests[0]!.text == "foreign code\n" &&
    requests[1]!.text == "foreign"

#guard requestKey "a:b" "c" != requestKey "a" "b:c"

run_cmd do
  let source := r#"open Verso Verso.Doc Verso.Genre.Manual VersoRosetta

#docs (Genre.Manual) mixedEndings "Mixed line endings" :=
:::::::
```lspCode "mixed" (context := "demo")
"# ++ "alpha\r😀beta\n" ++ r#"```

A named reference {lspRef "after" "alpha" (context := "demo")}[].
:::::::

#guard (collect mixedEndings.toPart).toOption.any fun requests =>
  if let #[body, reference] := requests then
    let first := body.origins[0]!.line
    body.text == "alpha\r😀beta\n" &&
      body.origins.map (·.line) == #[first, first + 1, first + 2] &&
      reference.origins.map (·.line) == #[first + 4]
  else false
"#
  let (_, messages) ← Lean.Elab.process source (← Lean.getEnv) (← Lean.getOptions)
  if messages.hasErrors then
    for message in messages.toList do Lean.logError message.data

end VersoRosettaTests

open Lean Rosetta

namespace VersoRosettaTests

#docs (Genre.Manual) unicodeDocument "Unicode and indentation" :=
:::::::
* A nested code block:

  ```lspCode "unicode" (context := "unicode")
  😀sample <>&
    βeta
  ```
:::::::

private def check (condition : Bool) (message : String) : IO Unit :=
  unless condition do throw <| IO.userError message

private def checkOrigins (request : Request) : IO Unit := do
  let lines := request.text.splitOn "\n" |>.toArray
  for (line, index) in lines.zipIdx do
    let origin := request.origins[index]!
    let some path := System.Uri.fileUriToPath? origin.uri
      | throw <| IO.userError "authored origin is not a file URI"
    let source ← IO.FS.readFile path
    let some (start, stop) := byteRange? source {
      start := ⟨origin.line, origin.character⟩
      «end» := ⟨origin.line, origin.character + line.utf16Length⟩ }
      | throw <| IO.userError "authored origin is outside its source"
    check (String.Pos.Raw.extract source ⟨start⟩ ⟨stop⟩ == line)
      "deindentation lost an exact authored line"

def run : IO Unit := IO.FS.withTempDir fun output => do
  let executable := (← IO.appPath).parent.getD "." / "rosetta-workflow"
  let emptyPath := output / "empty-path"
  IO.FS.createDir emptyPath
  let runWorkflow (stage : String) (input destination : System.FilePath) : IO Unit := do
    let result ← IO.Process.output {
      cmd := executable.toString
      args := #[stage, input.toString, destination.toString]
      env := if stage == "replay" then #[("PATH", some emptyPath.toString)] else #[] }
    check (result.exitCode == 0) s!"Workflow {stage} failed: {result.stdout}\n{result.stderr}"
  let analyzed := output / "analyzed"
  let saved := analyzed / "analysis.json"
  runWorkflow "analyze" "../tests/lsp/mock_server.py" analyzed
  runWorkflow "replay" saved (output / "replayed")
  let requests ← IO.ofExcept <| collect RosettaWorkflow.document
  check (requests.size == 4) "repeated inclusions were not collected"
  let bundle ← RosettaWorkflow.loadBundle saved
  check (bundle.requests.size == 2) "identical requests did not share analysis"
  check (bundle.contexts[0]?.any fun context => context.completion.getObjValD "kind" == toJson "full")
    "analysis did not save the mock's diagnostic report"
  check (bundle.requests.all fun snapshot => !snapshot.queries.isEmpty) "analysis did not collect queries"
  check (bundle.requests.all fun snapshot => match snapshot.tokens with
    | .ok spans => !spans.isEmpty | _ => false) "analysis did not produce semantic coloring"
  check (bundle.requests.all fun snapshot => snapshot.queries.all fun query => match query.hover with
    | .ok (some hover) => !hover.text.isEmpty | _ => false) "analysis did not produce successful hovers"
  -- Untrusted hover text must remain text even after saved-data replay.
  let bundle := { bundle with requests := bundle.requests.map fun snapshot =>
    { snapshot with queries := snapshot.queries.map fun query =>
      { query with hover := .ok (some { format := "plaintext", text := "<script>alert('hover')</script>&" }) } } }
  let escaped := output / "escaped.json"
  RosettaWorkflow.saveBundle escaped bundle
  let replay ← RosettaWorkflow.loadBundle escaped
  check (toJson replay == toJson bundle) "bundle round trip changed evidence"
  let some first := requests[0]? | throw <| IO.userError "missing test request"
  check (!(replay.find { first with text := first.text ++ "changed" }).isOk) "accepted changed source"
  check (!(replay.find { first with origins := first.origins.map fun o => { o with line := o.line + 1 } }).isOk)
    "accepted changed authored origins"
  check (!(enrich { replay with formatRevision := 1 } RosettaWorkflow.document).isOk) "accepted old bundle revision"
  let enriched ← IO.ofExcept <| enrich replay RosettaWorkflow.document
  check ((← IO.ofExcept <| collect enriched) == requests) "enrichment changed authored requests"
  runWorkflow "replay" escaped (output / "escaped")
  let unicodeRequests ← IO.ofExcept <| collect unicodeDocument.toPart
  let some unicode := unicodeRequests[0]? | throw <| IO.userError "missing Unicode request"
  check (unicode.text == "😀sample <>&\n  βeta\n") "nested code lost Unicode or indentation"
  checkOrigins unicode
  let unicodeBundle : Bundle := {
    contexts := #[{
      key := "unicode", languageId := "mock", diagnosticCoverage := "fixture"
      completion := .null, generatedDiagnostics := #[] }]
    requests := #[{
      request := unicode, languageId := "mock", diagnostics := #[], queries := #[]
      tokens := .ok #[{
        range := { start := ⟨0, 2⟩, «end» := ⟨0, 8⟩ }
        tokenType := "function", modifiers := #[] }] }] }
  let unicodePart ← IO.ofExcept <| enrich unicodeBundle unicodeDocument.toPart
  let code ← manualMain unicodePart (options := []) (config := {
    destination := output / "unicode", emitHtmlSingle := .immediately, emitHtmlMulti := .no, features := {} })
  check (code == 0) "Unicode HTML generation failed"
  let checked ← IO.Process.output {
    cmd := "python3", args := #["tests/check_rendering.py", output.toString] }
  check (checked.exitCode == 0) s!"HTML regression: {checked.stdout}\n{checked.stderr}"
  IO.println checked.stdout

end VersoRosettaTests

def main : IO Unit := VersoRosettaTests.run
