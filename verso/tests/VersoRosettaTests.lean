/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import VersoRosetta

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
