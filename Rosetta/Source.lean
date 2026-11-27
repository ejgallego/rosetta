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

private structure SourceLine where
  start : Nat
  stop : Nat
  length : Nat

/-- A reusable index of UTF-8 offsets and UTF-16 LSP positions. CR, LF and CRLF
are line endings, and index entries contain only the content of each line. -/
structure SourceIndex where
  private mk ::
  private contents : String
  private lines : Array SourceLine

@[inline] private def scanSourceLines (text : String) (init : α)
    (addLine : α → SourceLine → α) : α × SourceLine := Id.run do
  let mut acc := init
  let mut start := 0
  let mut offset := 0
  let mut length := 0
  let mut afterCR := false
  for c in text do
    if c == '\r' || c == '\n' then
      unless c == '\n' && afterCR do
        acc := addLine acc { start, stop := offset, length }
      start := offset + 1
      length := 0
    else
      length := length + c.utf16Size.toNat
    offset := offset + c.utf8Size
    afterCR := c == '\r'
  return (acc, { start, stop := offset, length })

def SourceIndex.ofString (text : String) : SourceIndex :=
  let (lines, last) := scanSourceLines text #[] Array.push
  { contents := text, lines := lines.push last }

/-- Compute the EOF position without retaining individual line bounds. -/
def Internal.sourceEndPosition (text : String) : Lsp.Position :=
  let (line, last) := scanSourceLines text 0 fun line _ => line + 1
  ⟨line, last.length⟩

instance : Inhabited SourceIndex := ⟨SourceIndex.ofString ""⟩

def SourceIndex.text (index : SourceIndex) : String := index.contents

def SourceIndex.lineCount (index : SourceIndex) : Nat := index.lines.size

/-- The line's UTF-16 length, excluding its terminator. -/
def SourceIndex.lineLength? (index : SourceIndex) (line : Nat) : Option Nat :=
  index.lines[line]?.map (·.length)

/-- The line's byte bounds, excluding its terminator. -/
def SourceIndex.lineByteRange? (index : SourceIndex) (line : Nat) : Option (Nat × Nat) :=
  index.lines[line]?.map fun line => (line.start, line.stop)

def SourceIndex.endPosition (index : SourceIndex) : Lsp.Position :=
  let line := index.lineCount - 1
  ⟨line, (index.lineLength? line).getD 0⟩

def SourceIndex.fullRange (index : SourceIndex) : Lsp.Range :=
  { start := ⟨0, 0⟩, «end» := index.endPosition }

/-- Reject positions beyond a line or inside a UTF-16 surrogate pair. -/
def SourceIndex.byteOffset? (index : SourceIndex) (pos : Lsp.Position) : Option Nat := Id.run do
  let some line := index.lines[pos.line]? | return none
  if pos.character == line.length then return some line.stop
  if pos.character > line.length then return none
  let mut col := 0
  let mut offset := line.start
  while col < pos.character do
    let c := String.Pos.Raw.get index.text ⟨offset⟩
    col := col + c.utf16Size.toNat
    offset := offset + c.utf8Size
  return if col == pos.character then some offset else none

/-- Reject offsets beyond the source or inside a UTF-8 character or CRLF terminator. -/
def SourceIndex.position? (index : SourceIndex) (offset : Nat) : Option Lsp.Position := Id.run do
  -- Find the last line starting at or before the offset.
  let mut lo := 0
  let mut hi := index.lineCount
  while lo + 1 < hi do
    let mid := (lo + hi) / 2
    let some line := index.lines[mid]? | return none
    if line.start ≤ offset then lo := mid else hi := mid
  let some line := index.lines[lo]? | return none
  if offset > line.stop then return none
  if offset == line.stop then return some ⟨lo, line.length⟩
  let mut byte := line.start
  let mut col := 0
  while byte < offset do
    let c := String.Pos.Raw.get index.text ⟨byte⟩
    byte := byte + c.utf8Size
    col := col + c.utf16Size.toNat
  return if byte == offset then some ⟨lo, col⟩ else none

def SourceIndex.byteRange? (index : SourceIndex) (range : Lsp.Range) : Option (Nat × Nat) := do
  guard (compare range.start range.end != .gt)
  let start ← index.byteOffset? range.start
  let stop ← index.byteOffset? range.end
  return (start, stop)

/-- Clamp oversized UTF-16 columns on existing lines. Reversed ranges, unknown
lines and positions that split a surrogate pair remain invalid. Returns the
clamped range and its byte bounds. -/
def SourceIndex.clampedByteRange? (index : SourceIndex) (range : Lsp.Range) :
    Option (Lsp.Range × Nat × Nat) := do
  guard (compare range.start range.end != .gt)
  let startLength ← index.lineLength? range.start.line
  let endLength ← index.lineLength? range.end.line
  let clamped := {
    start := { range.start with character := min range.start.character startLength }
    «end» := { range.end with character := min range.end.character endLength } }
  let (start, stop) ← index.byteRange? clamped
  return (clamped, start, stop)

def byteOffset? (text : String) (pos : Lsp.Position) : Option Nat :=
  (SourceIndex.ofString text).byteOffset? pos

def byteRange? (text : String) (range : Lsp.Range) : Option (Nat × Nat) :=
  (SourceIndex.ofString text).byteRange? range

structure SourceRange where
  uri : String
  source : SourceIndex
  range : Lsp.Range
deriving Inhabited

/-- Each mapped piece is an exact source slice. Deindentation can be represented
by one piece per line sharing the same source index. Wrappers are unmapped. -/
inductive Piece where
  | generated (text : String)
  | source (location : SourceRange)

private structure Segment where
  start : Nat
  stop : Nat
  origin? : Option (SourceRange × Nat)
deriving Inhabited

structure Projection where
  private mk ::
  private segments : Array Segment
  private index : SourceIndex

def Projection.sourceIndex (projection : Projection) : SourceIndex := projection.index

def Projection.text (projection : Projection) : String := projection.index.text

/-- Convert a UTF-8 byte offset in the composed text to an LSP position. -/
def Projection.position? (projection : Projection) (offset : Nat) : Option Lsp.Position :=
  projection.index.position? offset

def compose (pieces : Array Piece) : Except String Projection := do
  let mut text := ""
  let mut segments := #[]
  for piece in pieces do
    let (content, origin?) ← match piece with
      | .generated generated => pure (generated, none)
      | .source loc => do
        let some (start, stop) := loc.source.byteRange? loc.range
          | throw s!"invalid source range in {loc.uri}"
        pure ((String.Pos.Raw.extract loc.source.text ⟨start⟩ ⟨stop⟩), some (loc, start))
    if content.isEmpty && origin?.isNone then continue
    segments := segments.push {
      start := text.utf8ByteSize
      stop := text.utf8ByteSize + content.utf8ByteSize
      origin? }
    text := text ++ content
  return { segments, index := SourceIndex.ofString text }

structure MappedRange where
  locations : Array SourceRange := #[]
  containsGenerated : Bool := false

private def Projection.firstSegmentAfter (projection : Projection) (offset : Nat) : Nat := Id.run do
  let mut lo := 0
  let mut hi := projection.segments.size
  while lo < hi do
    let mid := (lo + hi) / 2
    if projection.segments[mid]!.stop ≤ offset then lo := mid + 1 else hi := mid
  return lo

private def Segment.mapRange (segment : Segment) (left right : Nat) (result : MappedRange) :
    Except String MappedRange := do
  let some (origin, sourceStart) := segment.origin? |
    return { result with containsGenerated := true }
  let some start := origin.source.position? (sourceStart + (left - segment.start))
    | throw "invalid projected source start"
  let some stop := origin.source.position? (sourceStart + (right - segment.start))
    | throw "invalid projected source end"
  let range : Lsp.Range := { start, «end» := stop }
  return { result with locations := result.locations.push { origin with range } }

private def Projection.mapByteRange (projection : Projection) (start stop : Nat) :
    Except String MappedRange := do
  if start == stop then
    let segment? := if start == projection.text.utf8ByteSize then projection.segments.back?
      else projection.segments[projection.firstSegmentAfter start]?
    let some segment := segment? | return {}
    return ← segment.mapRange start stop {}
  let mut result : MappedRange := {}
  for index in [projection.firstSegmentAfter start:projection.segments.size] do
    let segment := projection.segments[index]!
    if segment.start ≥ stop then break
    let left := max start segment.start
    let right := min stop segment.stop
    if left < right then result ← segment.mapRange left right result
  return result

/-- Split a range at projection boundaries instead of fabricating one source
location. Empty ranges belong to the following segment, or the last at EOF. -/
def Projection.mapRange (projection : Projection) (range : Lsp.Range) :
    Except String MappedRange := do
  let some (start, stop) := projection.index.byteRange? range
    | throw "invalid analysis range"
  projection.mapByteRange start stop

/-- Normalize a server range and project it back to the source. Oversized
UTF-16 columns on existing lines denote the end of those lines. -/
def Projection.mapServerRange (projection : Projection) (range : Lsp.Range) :
    Except String (Lsp.Range × MappedRange) := do
  let some (range, start, stop) := projection.index.clampedByteRange? range
    | throw "invalid server range"
  return (range, ← projection.mapByteRange start stop)

end Rosetta
