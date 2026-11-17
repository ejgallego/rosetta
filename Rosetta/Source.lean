/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
module

public import Lean.Data.Lsp.Utf16

public section

/-! Experimental source projection for external language analysis. -/
namespace Rosetta
open Lean

/-- A reusable line index for strict UTF-16 to UTF-8 conversion. -/
structure SourceIndex where
  private lines : Array (Nat × String)

def SourceIndex.ofString (text : String) : SourceIndex := Id.run do
  let lines := text.splitOn "\n" |>.toArray
  let mut indexed := #[]
  let mut base := 0
  for (line, index) in lines.zipIdx do
    let content := if index + 1 < lines.size then (line.dropSuffix "\r").copy else line
    indexed := indexed.push (base, content)
    base := base + line.utf8ByteSize + 1
  return { lines := indexed }

private def SourceIndex.byteOffsetCore? (index : SourceIndex) (pos : Lsp.Position)
    (clampColumn : Bool) : Option Nat := Id.run do
  let some (base, content) := index.lines[pos.line]? | return none
  let mut col := 0
  let mut offset := base
  for c in content.toList do
    if col == pos.character then return some offset
    col := col + c.utf16Size.toNat
    offset := offset + c.utf8Size
  return if col == pos.character || (clampColumn && col < pos.character) then some offset else none

/-- Rejects positions beyond a line or inside a UTF-16 surrogate pair.
CRLF is one line terminator; neither terminator code unit is source content. -/
def SourceIndex.byteOffset? (index : SourceIndex) (pos : Lsp.Position) : Option Nat :=
  index.byteOffsetCore? pos false

private def SourceIndex.byteRangeCore? (index : SourceIndex) (range : Lsp.Range)
    (clampColumn : Bool) : Option (Nat × Nat) := do
  guard (compare range.start range.end != .gt)
  let start ← index.byteOffsetCore? range.start clampColumn
  let stop ← index.byteOffsetCore? range.end clampColumn
  return (start, stop)

def SourceIndex.byteRange? (index : SourceIndex) (range : Lsp.Range) : Option (Nat × Nat) :=
  index.byteRangeCore? range false

def byteOffset? (text : String) (pos : Lsp.Position) : Option Nat :=
  (SourceIndex.ofString text).byteOffset? pos

def byteRange? (text : String) (range : Lsp.Range) : Option (Nat × Nat) :=
  (SourceIndex.ofString text).byteRange? range

structure SourceRange where
  uri : String
  source : String
  range : Lsp.Range
deriving Inhabited

/-- Each mapped piece is an exact source slice. Deindentation can be represented
by one piece per line; wrappers are ordinary unmapped pieces. -/
inductive Piece where
  | generated (text : String)
  | source (location : SourceRange)

private structure Segment where
  start : Nat
  stop : Nat
  origin? : Option (SourceRange × Nat)

structure Projection where
  text : String
  private segments : Array Segment
  private index : SourceIndex

def compose (pieces : Array Piece) : Except String Projection := do
  let mut text := ""
  let mut segments := #[]
  for piece in pieces do
    let (content, origin?) ← match piece with
      | .generated generated => pure (generated, none)
      | .source loc => do
        let some (start, stop) := byteRange? loc.source loc.range
          | throw s!"invalid source range in {loc.uri}"
        pure ((String.Pos.Raw.extract loc.source ⟨start⟩ ⟨stop⟩), some (loc, start))
    if content.isEmpty && origin?.isNone then continue
    segments := segments.push {
      start := text.utf8ByteSize
      stop := text.utf8ByteSize + content.utf8ByteSize
      origin? }
    text := text ++ content
  return { text, segments, index := SourceIndex.ofString text }

structure MappedRange where
  locations : Array SourceRange := #[]
  containsGenerated : Bool := false

private def Projection.mapByteRange (projection : Projection) (start stop : Nat) :
    MappedRange := Id.run do
  let pointIndex? := if start == stop then
      projection.segments.findIdx? (fun segment => segment.start ≤ start && start < segment.stop) <|>
        (if start == projection.text.utf8ByteSize && !projection.segments.isEmpty then
          some (projection.segments.size - 1) else none)
    else none
  let mut result : MappedRange := {}
  for (segment, index) in projection.segments.zipIdx do
    let ownsPoint := pointIndex? == some index
    let left := max start segment.start
    let right := min stop segment.stop
    if left < right || ownsPoint then
      match segment.origin? with
      | none => result := { result with containsGenerated := true }
      | some (origin, sourceStart) =>
        let fileMap := FileMap.ofString origin.source
        let range : Lsp.Range := {
          start := fileMap.utf8PosToLspPos ⟨sourceStart + left - segment.start⟩
          «end» := fileMap.utf8PosToLspPos ⟨sourceStart + right - segment.start⟩ }
        result := { result with locations := result.locations.push { origin with range } }
  return result

/-- Split a range at projection boundaries instead of fabricating one source
location. Empty ranges belong to the following segment, or the last at EOF. -/
def Projection.mapRange (projection : Projection) (range : Lsp.Range) :
    Except String MappedRange := do
  let some (start, stop) := projection.index.byteRange? range
    | throw "invalid analysis range"
  return projection.mapByteRange start stop

/-- Project a definition selection range returned for the composed document.
An oversized UTF-16 column on an existing line denotes the end of that line. -/
def Projection.mapDefinitionRange (projection : Projection) (range : Lsp.Range) :
    Except String MappedRange := do
  let some (start, stop) := projection.index.byteRangeCore? range true
    | throw "invalid definition selection range"
  return projection.mapByteRange start stop

end Rosetta
