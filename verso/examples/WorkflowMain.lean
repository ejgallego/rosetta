/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import RosettaWorkflow

def main (args : List String) : IO UInt32 := do
  match args with
  | ["analyze", server, output] =>
    let bundle ← (RosettaWorkflow.analyzeDocument server).toIO (IO.userError ∘ toString)
    IO.FS.createDirAll output
    RosettaWorkflow.saveBundle (System.FilePath.mk output / "analysis.json") bundle
    RosettaWorkflow.render bundle output
  | ["replay", saved, output] =>
    RosettaWorkflow.render (← RosettaWorkflow.loadBundle saved) output
  | _ => throw <| IO.userError "usage: rosetta-workflow analyze SERVER.py OUTPUT | replay BUNDLE.json OUTPUT"
