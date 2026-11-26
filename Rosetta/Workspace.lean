/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module
import Rosetta.FileUri
public import Rosetta.Client
public section
namespace Rosetta
open Lean

private def Client.requestWorkspaceSymbols (client : Client) (query : String) :
    EIO ClientError (Array Json) := do
  let result ← client.request "workspace/symbol" <| Json.mkObj [("query", toJson query)]
  if result == .null then return #[]
  EIO.ofExcept <| result.getArr?.mapError ClientError.protocol

private def decodeWorkspaceSymbol (value : Json) : Except String Lsp.SymbolInformation := do
  let .obj _ := value | throw "expected a workspace symbol object"
  -- LSP tags are optional; Lean's SymbolInformation decoder expects an array.
  let value := if value.getObjValD "tags" == .null then
      value.setObjVal! "tags" (.arr #[]) else value
  let symbol : Lsp.SymbolInformation ← fromJson? value
  unless compare symbol.location.range.start symbol.location.range.end != .gt do
    throw "workspace symbol location has a reversed range"
  return symbol

private def checkWorkspaceSymbolResolution (original : Json) (uri : Lsp.DocumentUri)
    (resolved : Lsp.SymbolInformation) : Except String Unit := do
  let name ← original.getObjValAs? String "name"
  let kind ← original.getObjValAs? Lsp.SymbolKind "kind"
  let container? ← original.getObjValAs? (Option String) "containerName"
  unless resolved.name == name && resolved.kind == kind && resolved.location.uri == uri &&
      container?.all (fun container => resolved.containerName? == some container) do
    throw "workspace symbol resolution changed the symbol identity or URI"

private def Client.resolveWorkspaceSymbol (client : Client) (value : Json) :
    EIO ClientError Lsp.SymbolInformation := do
  let location ← EIO.ofExcept <| (value.getObjVal? "location").mapError ClientError.protocol
  if (location.getObjVal? "range").isOk then
    return ← EIO.ofExcept <| (decodeWorkspaceSymbol value).mapError ClientError.protocol
  -- LSP WorkspaceSymbol may defer its location range until resolve.
  let uri ← EIO.ofExcept <| (location.getObjValAs? Lsp.DocumentUri "uri").mapError ClientError.protocol
  let response ← client.request "workspaceSymbol/resolve" value
  EIO.ofExcept <| (do
    let resolved ← decodeWorkspaceSymbol response
    checkWorkspaceSymbolResolution value uri resolved
    return resolved
    ).mapError ClientError.protocol

/-- Search the workspace and resolve deferred locations in all returned candidates. -/
def Client.workspaceSymbols (client : Client) (query : String) :
    EIO ClientError (Array Lsp.SymbolInformation) := do
  (← client.requestWorkspaceSymbols query).mapM client.resolveWorkspaceSymbol

/-- Ask for definitions at a position in an admitted document version. -/
def Document.definitions (document : Document) (position : Lsp.Position) :
    EIO ClientError (Array DefinitionTarget) := do
  let result ← document.request "textDocument/definition" <| Json.mkObj [
    ("textDocument", Json.mkObj [("uri", toJson document.uri)]), ("position", toJson position)]
  EIO.ofExcept <| (definitionTargets result).mapError ClientError.protocol

/-- Decode the server's semantic token legend against the exact opened text. -/
def Document.semanticTokens (document : Document) (range? : Option Lsp.Range := none) :
    EIO ClientError (Array SemanticSpan) := do
  let text ← document.text
  let caps ← document.client.capabilities
  let some provider := (caps.getObjVal? "semanticTokensProvider").toOption
    | throw <| ClientError.unsupported "semantic tokens"
  let useRange := range?.isSome && (provider.getObjValD "range" != .null) &&
    (provider.getObjValD "range" != .bool false)
  unless useRange || (provider.getObjValD "full" != .null &&
      provider.getObjValD "full" != .bool false) do
    throw <| ClientError.unsupported "full-document semantic tokens"
  let method := if useRange then "textDocument/semanticTokens/range" else "textDocument/semanticTokens/full"
  let params := [("textDocument", Json.mkObj [("uri", toJson document.uri)])]
  let params := if useRange then params ++ [("range", toJson range?)] else params
  let result ← document.request method (Json.mkObj params)
  EIO.ofExcept <| (semanticSpans text provider result).mapError ClientError.protocol

/-- Await retained notification params satisfying a readiness predicate. Configure
the method in retainedNotificationMethods before startup. The caller owns
server-specific readiness semantics; waiting is bounded. -/
def Client.waitForNotification (client : Client) (method : String) (accept : Json → Bool)
    (timeoutMs : Nat := 30000) : EIO ClientError Unit := do
  unless client.config.retainedNotificationMethods.contains method do
    throw <| ClientError.invalidInput s!"notification method was not configured for retention: {method}"
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if let some params ← client.latestNotification method then
      if accept params then return
    if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline method
    IO.sleep 20

private def readSource (uri : String) : EIO ClientError String := do
  let some file := localFileUriPath? uri
    | throw <| ClientError.unsupported s!"reading source without a local file path: {uri}"
  IO.FS.readFile file

/-- Use a live overlay when available; only unopened files are read from disk. -/
private def withSourceDocument (client : Client) (uri languageId : String)
    (action : Document → EIO ClientError α) : EIO ClientError α := do
  if let some document ← client.document? uri then
    return ← document.withBorrowed languageId action
  client.withDocument uri languageId (← readSource uri) action

/-- Find distinct indexed locations for an exact name and optional container,
in server order. The result can be empty or contain several locations. -/
def Client.lookupSymbols (client : Client) (name : String)
    (container? : Option String := none) : EIO ClientError (Array Lsp.Location) := do
  if name.isEmpty then throw <| ClientError.invalidInput "empty symbol name"
  if container?.any String.isEmpty then throw <| ClientError.invalidInput "empty symbol container"
  let mut locations : Array Lsp.Location := #[]
  for value in ← client.requestWorkspaceSymbols name do
    -- Fuzzy hits may require resolution, so filter on the raw identity first.
    let candidateName ← EIO.ofExcept <| (value.getObjValAs? String "name").mapError ClientError.protocol
    unless candidateName == name do continue
    if let some container := container? then
      let candidateContainer? ← EIO.ofExcept <|
        (value.getObjValAs? (Option String) "containerName").mapError ClientError.protocol
      unless candidateContainer? == some container do continue
    let symbol ← client.resolveWorkspaceSymbol value
    unless locations.contains symbol.location do
      locations := locations.push symbol.location
  return locations

/-- Find the unique indexed location for an exact name and optional container. -/
def Client.lookupSymbol (client : Client) (name : String)
    (container? : Option String := none) : EIO ClientError Lsp.Location := do
  let locations ← client.lookupSymbols name container?
  if let #[location] := locations then return location
  let description := container?.elim name (fun container => s!"{name} in {container}")
  throw <| ClientError.invalidInput s!"Expected one workspace symbol for {description}, found {locations.size}"

/-- A lossless source slice with optional LSP semantic coloring. -/
structure HighlightedPiece where
  text : String
  tokenType : String := ""
  modifiers : Array String := #[]
  deriving Inhabited, ToJson, FromJson

/-- The exact text of a source range, with semantic coloring when available. -/
structure SourceExcerpt where
  location : Lsp.Location
  languageId : String
  pieces : Array HighlightedPiece
  deriving ToJson, FromJson

/-- Read the exact source range. Return plain text when semantic tokens are unsupported. -/
def Client.sourceExcerpt (client : Client) (languageId : String) (location : Lsp.Location) :
    EIO ClientError SourceExcerpt := do
  withSourceDocument client location.uri languageId fun document => do
    let text ← document.text
    let index := SourceIndex.ofString text
    let some (range, start, stop) := index.clampedByteRange? location.range
      | throw <| ClientError.protocol "invalid source range"
    let spans ← try document.semanticTokens (some range) catch error =>
      match error with
      | .unsupported _ => pure #[]
      | _ => throw error
    let mut pieces := #[]
    let mut offset := start
    for span in spans do
      let some (left, right) := index.byteRange? span.range
        | throw <| ClientError.protocol "invalid semantic token range"
      let left := max start left
      let right := min stop right
      if left ≥ right then continue
      if offset < left then
        pieces := pieces.push { text := String.Pos.Raw.extract text ⟨offset⟩ ⟨left⟩ }
      pieces := pieces.push {
        text := String.Pos.Raw.extract text ⟨left⟩ ⟨right⟩
        tokenType := span.tokenType, modifiers := span.modifiers }
      offset := right
    if offset < stop then pieces := pieces.push { text := String.Pos.Raw.extract text ⟨offset⟩ ⟨stop⟩ }
    return { location, languageId, pieces }

/-- Look up a workspace symbol and read its indexed location. -/
def Client.symbolSource (client : Client) (languageId name : String)
    (container? : Option String := none) : EIO ClientError SourceExcerpt := do
  let location ← client.lookupSymbol name container?
  client.sourceExcerpt languageId location

end Rosetta
