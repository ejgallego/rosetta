/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
module

public import Rosetta.Workspace
public import Rosetta.Document

public section

namespace Rosetta
open Lean

structure DiagnosticEvidence where
  coverage : String
  completion : Json := .null
  diagnostics : Array Lsp.Diagnostic

/-- Profiles supply valid project membership, reference syntax, and diagnostic
readiness. No language switch or process defaults live in the analysis core.
The document must already belong to the configured project; it is opened with
an unsaved overlay and is never rewritten on disk by this library. Relative
document paths resolve against the client's canonical project root. -/
structure ContextProfile where
  key : String
  languageId : String
  client : ClientConfig
  document : System.FilePath
  referencePrefix : String
  referenceSuffix : String
  /-- The UTF-8 byte offset of a character inside the authored reference. -/
  referenceQueryOffset : String → Nat := fun _ => 0
  provenance : Json := .null
  /-- Collect profile-specific evidence, observing the analysis token during waits. -/
  collectDiagnostics : Document → Option Std.CancellationToken → EIO ClientError DiagnosticEvidence

/-- Live controls for an analysis run. -/
structure AnalysisOptions where
  /-- Upper bound on eager hover/definition query pairs in each code block.
  Explicit reference queries do not consume this budget. -/
  maxQueriesPerCodeBlock : Nat := 32
  /-- Deadline for each feature request, defaulting to `ClientConfig.timeoutMs`. -/
  requestTimeoutMs? : Option Nat := none
  /-- Checked between analysis phases and passed to diagnostic collection and feature requests. -/
  cancel? : Option Std.CancellationToken := none

private def AnalysisOptions.checkCancelled (options : AnalysisOptions) : EIO ClientError Unit := do
  if let some token := options.cancel? then
    if ← token.isCancelled then throw <| ClientError.cancelled "analysis"

private structure Placement where
  request : Request
  reference? : Option (Lsp.Position × Lsp.Range)

private structure PreparedContext where
  profile : ContextProfile
  projection : Projection
  placements : Array Placement

private def prepare (profile : ContextProfile) (requests : Array Request) :
    Except String PreparedContext := do
  let mut pieces := #[]
  let mut offset := 0
  let mut pending : Array (Request × Option (Nat × Lsp.Range)) := #[]
  for request in requests do
    let sourceIndex ← request.checkedSourceIndex
    let range := sourceIndex.fullRange
    let beforeText := if request.kind == .reference then profile.referencePrefix else ""
    let suffix := if request.kind == .reference then profile.referenceSuffix else ""
    pieces := pieces.push (.generated beforeText)
    offset := offset + beforeText.utf8ByteSize
    let reference? ← if request.kind == .reference then do
        let bodyOffset := profile.referenceQueryOffset request.text
        unless bodyOffset < request.text.utf8ByteSize do
          throw "reference query offset is outside its text"
        unless (request.text.pos? ⟨bodyOffset⟩).isSome do
          throw "reference query offset is inside a UTF-8 character"
        pure (some (offset + bodyOffset, range))
      else pure none
    pending := pending.push (request, reference?)
    pieces := pieces.push (.source {
      uri := request.key, source := sourceIndex, range })
    pieces := pieces.push (.generated (suffix ++ "\n"))
    offset := offset + request.text.utf8ByteSize + suffix.utf8ByteSize + 1
  let projection ← compose pieces
  let placements ← pending.mapM fun (request, reference?) => do
    let reference? ← reference?.mapM fun (offset, range) => do
      let some position := projection.position? offset
        | throw "reference query offset is not an LSP character boundary"
      pure (position, range)
    return { request, reference? }
  return { profile, projection, placements }

private def attempt (action : EIO ClientError α) : EIO ClientError (Outcome α) := do
  try return .ok (← action)
  catch error =>
    match error with
    | .unsupported _ => return .unsupported
    | .server .. => return .failed error.toString
    | _ => throw error

private def supports (capabilities : Json) (key : String) : Bool :=
  match capabilities.getObjVal? key with
  | .error _ | .ok .null | .ok (.bool false) => false
  | _ => true

private def query (document : Document) (caps : Json) (projection : Projection)
    (position : Lsp.Position) (range : Lsp.Range) (options : AnalysisOptions) :
    EIO ClientError QuerySnapshot := do
  options.checkCancelled
  let uri := document.uri
  let timeoutMs := options.requestTimeoutMs?.getD document.client.config.timeoutMs
  let hover ← if supports caps "hoverProvider" then attempt do
      let params := Json.mkObj [
        ("textDocument", Json.mkObj [("uri", toJson uri)]), ("position", toJson position)]
      let result ← document.request "textDocument/hover" params timeoutMs options.cancel?
      EIO.ofExcept <| (decodeHover result).mapError ClientError.protocol
    else pure .unsupported
  options.checkCancelled
  let definitions ← if supports caps "definitionProvider" then
      attempt (document.definitions position timeoutMs options.cancel?)
    else pure .unsupported
  let mut localTargets := #[]
  if let .ok targets := definitions then
    for target in targets do
      if target.uri != uri then
        let targetDocument? ← document.client.document? target.uri
        unless targetDocument?.any (fun target => target.uri == uri && target.version == document.version) do
          continue
      if let .ok (_, mapped) := projection.mapServerRange target.selectionRange then
        for location in mapped.locations do
          unless localTargets.contains location.uri do localTargets := localTargets.push location.uri
  return { range, hover, definitions, localTargets }

-- Prepending keeps insertion constant-time. Consumers reverse each group once.
private def prependFor (groups : Std.HashMap String (List α)) (key : String) (value : α) :
    Std.HashMap String (List α) :=
  groups.alter key fun entries => some (value :: entries.getD [])

private structure ProjectedEvidence where
  diagnostics : Std.HashMap String (List Lsp.Diagnostic) := {}
  generatedDiagnostics : List Lsp.Diagnostic := []
  spans : Std.HashMap String (List (SemanticSpan × Lsp.Position)) := {}

private def ProjectedEvidence.addDiagnostic (result : ProjectedEvidence) (projection : Projection)
    (diagnostic : Lsp.Diagnostic) : Except String ProjectedEvidence := do
  let (normalized, mapped) ← projection.mapServerRange diagnostic.range
  let mut diagnostics := result.diagnostics
  for location in mapped.locations do
    diagnostics := prependFor diagnostics location.uri { diagnostic with range := location.range }
  let generatedDiagnostics := if mapped.containsGenerated || mapped.locations.isEmpty then
    { diagnostic with range := normalized } :: result.generatedDiagnostics else result.generatedDiagnostics
  return { result with diagnostics, generatedDiagnostics }

private def ProjectedEvidence.addSpan (result : ProjectedEvidence) (projection : Projection)
    (token : SemanticSpan) : Except String ProjectedEvidence := do
  let mapped ← projection.mapRange token.range
  if mapped.containsGenerated then return result
  let mut spans := result.spans
  for location in mapped.locations do
    spans := prependFor spans location.uri ({ token with range := location.range }, token.range.start)
  return { result with spans }

private def analyzeContext (client : Client) (prepared : PreparedContext) (options : AnalysisOptions) :
    EIO ClientError (Array RequestSnapshot × ContextObservation) := do
  let ⟨profile, projection, placements⟩ := prepared
  options.checkCancelled
  let path := if profile.document.isAbsolute then profile.document else client.config.root / profile.document
  let uri := (System.Uri.pathToUri path : String)
  client.withDocument uri profile.languageId projection.text fun document => do
    options.checkCancelled
    let evidence ← profile.collectDiagnostics document options.cancel?
    options.checkCancelled
    let caps ← client.capabilities
    let timeoutMs := options.requestTimeoutMs?.getD client.config.timeoutMs
    let tokens ← attempt <| document.semanticTokens (timeoutMs := timeoutMs) (cancel? := options.cancel?)
      (sourceIndex? := some projection.sourceIndex)
    options.checkCancelled
    let mut projected : ProjectedEvidence := {}
    for diagnostic in evidence.diagnostics do
      options.checkCancelled
      projected ← EIO.ofExcept <| (projected.addDiagnostic projection diagnostic).mapError ClientError.protocol
    if let .ok tokens := tokens then
      for token in tokens do
        options.checkCancelled
        projected ← EIO.ofExcept <| (projected.addSpan projection token).mapError ClientError.protocol
    let mut snapshots := #[]
    for placement in placements do
      options.checkCancelled
      let request := placement.request
      let spans := (projected.spans[request.key]?.getD []).reverse
      let localTokens := match tokens with
        | .ok _ => Outcome.ok (spans.toArray.map (·.1))
        | .unsupported => .unsupported
        | .failed message => .failed message
      let mut queries := #[]
      if let some (position, range) := placement.reference? then
        queries := queries.push (← query document caps projection position range options)
      else
        -- Bound eager hover/navigation work. No token support means plain code,
        -- while explicit reference queries continue to work independently.
        for (token, position) in spans do
          if queries.size ≥ options.maxQueriesPerCodeBlock then break
          unless #["function", "method", "variable", "parameter", "type", "namespace"].contains token.tokenType do
            continue
          queries := queries.push (← query document caps projection position token.range options)
      snapshots := snapshots.push {
        request, languageId := profile.languageId, tokens := localTokens, queries
        diagnostics := (projected.diagnostics[request.key]?.getD []).reverse.toArray }
    options.checkCancelled
    return (snapshots, {
      key := profile.key, languageId := profile.languageId,
      diagnosticCoverage := evidence.coverage, completion := evidence.completion,
      provenance := profile.provenance, generatedDiagnostics := projected.generatedDiagnostics.reverse.toArray })

private def uniqueRequests (requests : Array Request) (options : AnalysisOptions) :
    EIO ClientError (Array Request) := do
  let mut unique := #[]
  let mut seen : Std.HashMap String Request := {}
  for request in requests do
    options.checkCancelled
    if let some previous := seen[request.key]? then
      unless previous == request do throw <| ClientError.invalidInput s!"conflicting request identity: {request.key}"
    else
      seen := seen.insert request.key request
      unique := unique.push request
  return unique

private def preparePlan (profiles : Array ContextProfile) (requests : Array Request)
    (options : AnalysisOptions) : EIO ClientError (Array PreparedContext) := do
  let mut contexts := #[]
  let mut groups : Std.HashMap String (List Request) := {}
  for request in requests do
    options.checkCancelled
    unless groups.contains request.context do contexts := contexts.push request.context
    groups := prependFor groups request.context request
  let mut plan := #[]
  for context in contexts do
    options.checkCancelled
    let #[profile] := profiles.filter (·.key == context)
      | throw <| ClientError.invalidInput s!"expected one profile for context {context}"
    let prepared ← EIO.ofExcept <|
      (prepare profile (groups[context]?.getD []).reverse.toArray).mapError ClientError.invalidInput
    plan := plan.push prepared
  return plan

private def checkedBundle (bundle : Bundle) (options : AnalysisOptions) : EIO ClientError Bundle := do
  options.checkCancelled
  EIO.ofExcept <| bundle.validate.mapError ClientError.invalidInput
  options.checkCancelled
  return bundle

/-- Analyze one context in a caller-owned initialized session. The profile
configuration must match the client's, after canonicalizing the root, and all
requests must name this profile's context. A temporary document is scoped to
analysis. An identical open overlay for the same local file is borrowed with
its original URI and admitted version.
The diagnostic callback receives the analysis token and owns its waits. -/
def analyzeWithClient (client : Client) (profile : ContextProfile) (requests : Array Request)
    (options : AnalysisOptions := {}) : EIO ClientError Bundle := do
  options.checkCancelled
  unless client.config == profile.client do
    let mismatch := ClientError.invalidInput "analysis profile does not match the initialized client configuration"
    unless client.config == { profile.client with root := client.config.root } do throw mismatch
    let root ← match ← (IO.FS.realPath profile.client.root).toBaseIO with
      | .ok root => pure root
      | .error (.noFileOrDirectory ..) | .error (.inappropriateType ..) => throw mismatch
      | .error error => throw <| ClientError.io error
    unless root == client.config.root do throw mismatch
  let unique ← uniqueRequests requests options
  unless unique.all (·.context == profile.key) do
    throw <| ClientError.invalidInput "analysis requests must match the session profile context"
  if unique.isEmpty then return ← checkedBundle { requests := #[], contexts := #[] } options
  let prepared ← EIO.ofExcept <| (prepare profile unique).mapError ClientError.invalidInput
  let (snapshots, observation) ← analyzeContext client prepared options
  checkedBundle { requests := snapshots, contexts := #[observation] } options

/-- Analyze a complete plan once at the generator boundary. Duplicate identical
requests are shared; conflicting identities and unregistered contexts fail.
Prepare all used contexts before creating sessions. Initialization uses the
client configuration, and diagnostic callbacks own their waits.
Use `analyzeWithClient` to reuse an initialized session. -/
def analyze (profiles : Array ContextProfile) (requests : Array Request)
    (options : AnalysisOptions := {}) : EIO ClientError Bundle := do
  options.checkCancelled
  let unique ← uniqueRequests requests options
  let plan ← preparePlan profiles unique options
  let mut bundle : Bundle := { requests := #[], contexts := #[] }
  for prepared in plan do
    options.checkCancelled
    let (snapshots, observation) ← Client.withSession prepared.profile.client fun client =>
      analyzeContext client prepared options
    bundle := { requests := bundle.requests ++ snapshots, contexts := bundle.contexts.push observation }
  checkedBundle bundle options

end Rosetta
