/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
import VersoRosetta.Render
import VersoManual

namespace VersoRosetta
open Lean Lean.Elab Verso Verso.Doc Verso.Doc.Elab Verso.Genre.Manual Rosetta
open Verso.Genre (Manual)

/-- Each phase owns the request exactly once, including its source text. -/
inductive Payload where
  | pending (request : Request)
  | analyzed (snapshot : RequestSnapshot)
deriving ToJson, FromJson

def Payload.request : Payload → Request
  | .pending request => request
  | .analyzed snapshot => snapshot.request

def Payload.snapshot? : Payload → Option RequestSnapshot
  | .pending _ => none
  | .analyzed snapshot => some snapshot

deriving instance Quote for LineOrigin
deriving instance Quote for RequestKind
deriving instance Quote for Request

/-- Logical identity is independent of delimiters and rendered occurrences. -/
def requestKey (context key : String) : String := (toJson (context, key)).compress

private def requestDomain : Name := ``Rosetta.Request

private def localLink (state : TraverseState) (key : String) : Option String := do
  let object ← state.getDomainObject? requestDomain key
  let id ← object.ids.toArray[0]?
  return (← state.resolveId id).relativeLink

/-- Occurrence IDs belong to Verso; logical targets choose the first occurrence. -/
private def registerRequest (id : InternalId) (data : Json) : ReaderT TraverseContext (StateT TraverseState (BuildLogT IO)) Unit := do
  let payload ← match fromJson? (α := Payload) data with
    | .ok payload => pure payload
    | .error message => Verso.reportError message; return
  let path ← (·.path) <$> read
  discard <| externalTag id path s!"rosetta-{payload.request.key}"
  modify fun state =>
    if (state.getDomainObject? requestDomain payload.request.key).isSome then state
    else state.saveDomainObject requestDomain payload.request.key id

block_extension Block.lspContexts (contexts : Array ContextObservation) where
  data := toJson contexts
  traverse _ _ _ := pure none
  extraCss := [CSS.mk css]
  toHtml := some <| fun _ _ _ data _ => do
    match fromJson? (α := Array ContextObservation) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok contexts =>
      return Verso.Output.Html.tag "aside" #[("class", "lsp-contexts")] <| .seq <| contexts.map fun context =>
        Verso.Output.Html.tag "details" #[] <| .seq #[
          Verso.Output.Html.tag "summary" #[] (.text true s!"Analysis context: {context.key}"),
          Verso.Output.Html.tag "p" #[] (.text true context.diagnosticCoverage),
          Verso.Output.Html.tag "ul" #[] <| .seq <| context.generatedDiagnostics.map fun diagnostic =>
            Verso.Output.Html.tag "li" #[] (.text true s!"Context diagnostic: {diagnostic.message}")]
  toTeX := some <| fun _ go _ data _ => do
    match fromJson? (α := Array ContextObservation) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok contexts => go (.para #[.text <| String.intercalate "\n" <|
        contexts.toList.map (fun context => s!"{context.key}: {context.diagnosticCoverage}")])

block_extension Block.lspCode (request : Request) via withHighlighting where
  data := toJson (Payload.pending request)
  traverse id data _ := do
    registerRequest id data
    pure none
  extraCss := [CSS.mk css]
  extraJs := [JS.mk hoverJs]
  toHtml := some <| fun _ _ id data _ => do
    match fromJson? (α := Payload) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok payload =>
      let state ← Verso.Doc.Html.HtmlT.state
      liftM <| codeHtml (g := Manual) payload.request payload.snapshot? (state.htmlId id) (localLink state)
  toTeX := some <| fun _ go _ data _ => do
    match fromJson? (α := Payload) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok payload =>
      let code ← go (.code payload.request.text)
      let diagnostics ← match payload.snapshot? with
        | none => pure Verso.Output.TeX.empty
        | some snapshot =>
          go (.para #[.text <| String.intercalate "\n" <|
            snapshot.diagnostics.toList.map (·.message)])
      return code ++ diagnostics

inline_extension Inline.lspReference (request : Request) via withHighlighting where
  data := toJson (Payload.pending request)
  traverse id data _ := do
    registerRequest id data
    pure none
  extraCss := [CSS.mk css]
  extraJs := [JS.mk hoverJs]
  toHtml := some <| fun _ id data _ => do
    match fromJson? (α := Payload) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok payload =>
      let state ← Verso.Doc.Html.HtmlT.state
      liftM <| referenceHtml (g := Manual) payload.request payload.snapshot? (state.htmlId id) (localLink state)
  toTeX := some <| fun go _ data _ => do
    match fromJson? (α := Payload) data with
    | .error message => Verso.reportError message *> pure .empty
    | .ok payload => go (.code payload.request.text)

structure CodeConfig where
  key : String
  context : String

structure ReferenceConfig extends CodeConfig where
  written : Verso.ArgParse.WithSyntax String

section
variable [Monad m] [MonadInfoTree m] [MonadResolveName m] [MonadEnv m] [MonadError m]

instance : Verso.ArgParse.FromArgs CodeConfig m where
  fromArgs := CodeConfig.mk <$> .positional `key .string <*> .named' `context false

instance : Verso.ArgParse.FromArgs ReferenceConfig m where
  fromArgs := (fun key written context => { key, context, written }) <$>
    .positional `key .string <*> .positional `written (.withSyntax .string) <*> .named' `context false
end

private initialize authoredSourceIndex : EnvExtension (Option SourceIndex) ←
  registerEnvExtension (pure none) (asyncMode := .local)

private def getAuthoredSourceIndex (source : String) : DocElabM SourceIndex := do
  if let some index := authoredSourceIndex.getState (← getEnv) then
    if index.text == source then return index
  let index := SourceIndex.ofString source
  modifyEnv fun env => authoredSourceIndex.setState env (some index)
  return index

private def codeOrigins (contents : StrLit) : DocElabM (Array LineOrigin) := do
  let some rawRange := contents.raw.getRange? | throwError "LSP code requires source positions"
  let fileMap ← getFileMap
  let uri := (System.Uri.pathToUri (← getFileName) : String)
  let raw := rawRange.start.extract fileMap.source rawRange.stop
  let text := contents.getString
  let rawIndex := SourceIndex.ofString raw
  let textIndex := SourceIndex.ofString text
  let fileIndex ← getAuthoredSourceIndex fileMap.source
  unless rawIndex.lineCount == textIndex.lineCount do
    throwError "code source projection has different line counts"
  let mut origins := #[]
  for line in [:textIndex.lineCount] do
    let some (rawStart, rawStop) := rawIndex.lineByteRange? line
      | throwError "code projection has an invalid authored line"
    let some (start, stop) := textIndex.lineByteRange? line
      | throwError "code projection has an invalid text line"
    let rawLine := String.Pos.Raw.extract raw ⟨rawStart⟩ ⟨rawStop⟩
    let textLine := String.Pos.Raw.extract text ⟨start⟩ ⟨stop⟩
    unless rawLine.endsWith textLine do throwError "code text is not an exact authored line suffix"
    let removedIndent := rawLine.dropEnd textLine.length |>.copy
    unless removedIndent.all Char.isWhitespace do throwError "code projection removed non-whitespace source"
    let offset := rawRange.start.byteIdx + rawStart + removedIndent.utf8ByteSize
    let some position := fileIndex.position? offset | throwError "invalid authored code offset"
    origins := origins.push { uri, line := position.line, character := position.character }
  return origins

/-- Experimental syntax: compilation records intent and never starts a server. -/
@[code_block]
def lspCode : CodeBlockExpanderOf CodeConfig
  | config, contents => do
    let request : Request := {
      key := requestKey config.context config.key, context := config.context
      kind := .code, text := contents.getString, origins := ← codeOrigins contents }
    ``(Block.other (Block.lspCode $(quote request)) #[])

/-- Named references are compiled as inert requests in an explicitly named scope. -/
@[role]
def lspRef : RoleExpanderOf ReferenceConfig
  | config, contents => do
    unless contents.isEmpty do throwError "lspRef displays its written name; its body must be empty"
    let text := config.written.val
    if text.isEmpty || (SourceIndex.ofString text).lineCount != 1 then
      throwError "expected a nonempty single-line reference"
    let some range := config.written.syntax.getRange? | throwError "reference requires source positions"
    let fileMap ← getFileMap
    let raw := range.start.extract fileMap.source range.stop
    unless raw == "\"" ++ text ++ "\"" do
      throwError "escaped reference literals are not supported by this experimental source mapper"
    let sourceIndex ← getAuthoredSourceIndex fileMap.source
    let some position := sourceIndex.position? (range.start.byteIdx + 1)
      | throwError "invalid authored reference offset"
    let request : Request := {
      key := requestKey config.context config.key, context := config.context
      kind := .reference, text
      origins := #[{ uri := (System.Uri.pathToUri (← getFileName) : String),
                     line := position.line, character := position.character }] }
    ``(Inline.other (Inline.lspReference $(quote request)) #[])

/-- Transform extension children as well as ordinary blocks. Other extensions'
opaque JSON is left alone; nested document data requires its own explicit hook. -/
private partial def mapPartM [Monad m] [MonadExceptOf String m] (f : Payload → m Payload)
    (part : Part Manual) : m (Part Manual) := do
  let onInline := fun (recurse : Verso.Doc.Inline Manual → m (Verso.Doc.Inline Manual))
      (container : Verso.Genre.Manual.Inline) (children : Array (Verso.Doc.Inline Manual)) => do
    let container ← if container.name == ``Inline.lspReference then
        let payload ← match fromJson? (α := Payload) container.data with
          | .ok payload => pure payload
          | .error message => throw message
        let payload ← f payload
        pure { container with data := toJson payload }
      else pure container
    return Verso.Doc.Inline.other container (← children.mapM recurse)
  let onBlock := fun (_ : Verso.Doc.Inline Manual → m (Verso.Doc.Inline Manual))
      (recurse : Verso.Doc.Block Manual → m (Verso.Doc.Block Manual))
      (container : Verso.Genre.Manual.Block) (children : Array (Verso.Doc.Block Manual)) => do
    let container ← if container.name == ``Block.lspCode then
        let payload ← match fromJson? (α := Payload) container.data with
          | .ok payload => pure payload
          | .error message => throw message
        let payload ← f payload
        pure { container with data := toJson payload }
      else pure container
    return Verso.Doc.Block.other container (← children.mapM recurse)
  return { part with
    title := ← part.title.mapM (·.rewriteOtherM onInline)
    content := ← part.content.mapM (·.rewriteOtherM onInline onBlock)
    subParts := ← part.subParts.mapM (mapPartM f) }

def collect (part : Part Manual) : Except String (Array Request) := do
  let action := mapPartM (m := StateT (Array Request) (Except String)) (fun payload => do
    modify (·.push payload.request)
    return payload) part
  return (← action.run #[]).2

/-- Replay accepted snapshots against exactly matching compiled requests.
This is an explicit offline artifact operation, not an automatic cache lookup. -/
def enrich (bundle : Bundle) (part : Part Manual) : Except String (Part Manual) := do
  bundle.validate
  let requests ← collect part
  let part ← mapPartM (fun payload => do
    let snapshot ← bundle.find payload.request
    return Payload.analyzed snapshot) part
  let contexts := bundle.contexts.filter fun context => requests.any (·.context == context.key)
  let content := part.content.filter fun
    | .other container _ => container.name != ``Block.lspContexts
    | _ => true
  return { part with content := #[.other (Block.lspContexts contexts) #[]] ++ content }

end VersoRosetta
