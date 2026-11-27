/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
module

public import Rosetta.Framing
public import Rosetta.Protocol
public import Std.Sync.Mutex
public import Std.Sync.CancellationToken

public section

/-! Experimental stdio client. Transport state never protects blocking IO.
Document handles belong to one client and one admitted document version. -/
namespace Rosetta
open Lean

structure ClientConfig where
  command : String
  args : Array String := #[]
  root : System.FilePath
  env : Array (String × Option String) := #[]
  initializationOptions : Json := .null
  configuration : Array (String × Json) := #[]
  experimentalCapabilities : Json := .null
  /-- General notification methods whose latest params survive journal eviction.
  Configure before startup to include notifications sent during initialization. -/
  retainedNotificationMethods : Array String := #[]
  timeoutMs : Nat := 5000
  maxQueuedMessages : Nat := 128
  maxQueuedBytes : Nat := 8 * 1024 * 1024
  maxPendingRequests : Nat := 128
  maxInboundBytes : Nat := 16 * 1024 * 1024

def stdio : IO.Process.StdioConfig := {
  stdin := .piped, stdout := .piped, stderr := .piped }

private structure Outbound where
  payload : String
  requestId? : Option Nat := none
  deadline : Nat

private structure State where
  nextId : Nat := 0
  nextVersion : Int := 1
  pending : Std.HashMap Nat (Option (Except ClientError Json)) := {}
  documents : Std.HashMap String OpenDocument := {}
  queue : Array Outbound := #[]
  queuedBytes : Nat := 0
  active? : Option Outbound := none
  stopWriter : Bool := false
  latestNotifications : Std.HashMap String Json := {}
  notifications : Array Json := #[]
  droppedNotifications : Nat := 0
  failure? : Option ClientError := none
  closing : Bool := false
  writeTerminationRequested : Bool := false

structure Client where
  private mk ::
  private child : IO.Process.Child { stdio with stdin := .null }
  private state : Std.Mutex State
  private reader : Task (Except ClientError Unit)
  private writer : Task (Except ClientError Unit)
  private watchdog : Task Bool
  private finishWatchdog : IO.Promise Bool
  private terminationErrors : IO.Ref (Array String)
  private stderrReader : Task (Except IO.Error Unit)
  private stderrBytes : IO.Ref ByteArray
  private initializeResult : IO.Ref Json
  config : ClientConfig
  private cleanup : IO.Promise CloseReport

/-- A document version in its owning session. Obtain a new handle after an edit;
closing/reopening a URI never revives an older handle. -/
structure Document where
  private mk ::
  client : Client
  uri : String
  version : Int

private def fail (s : State) (error : ClientError) : State :=
  { s with failure? := s.failure? <|> some error, queue := #[], queuedBytes := 0, stopWriter := true }

private def admit (s : State) (internal : Bool := false) : EIO ClientError Unit := do
  if s.closing && !internal then throw ClientError.closed
  if let some failure := s.failure? then throw failure

private def enqueue (config : ClientConfig) (s : State) (frame : Outbound) : EIO ClientError State := do
  if s.stopWriter then throw ClientError.disconnected
  if s.queue.size ≥ config.maxQueuedMessages ||
      s.queuedBytes + frame.payload.utf8ByteSize > config.maxQueuedBytes then
    throw ClientError.queueFull
  return { s with queue := s.queue.push frame, queuedBytes := s.queuedBytes + frame.payload.utf8ByteSize }

private def checkedDocument (s : State) (uri : String) (version : Int) : EIO ClientError OpenDocument := do
  let some doc := s.documents[uri]? | throw <| ClientError.staleDocument uri version none
  unless doc.version == version do throw <| ClientError.staleDocument uri version (some doc.version)
  return doc

private def checkDocument (s : State) (document? : Option (String × Int)) : EIO ClientError Unit := do
  if let some (uri, expected) := document? then
    discard <| checkedDocument s uri expected

private def protocol (value : Except String α) : EIO ClientError α :=
  EIO.ofExcept (value.mapError ClientError.protocol)

private def serializeCall (message : Option Json.Structured → JsonRpc.Message)
    (params? : Option Json) : String :=
  let json := match params? with
    | none => toJson (message none)
    | some (.arr items) => toJson (message (some (.arr items)))
    | some (.obj fields) => toJson (message (some (.obj fields)))
    -- The public client API also accepts explicit null and scalar params.
    | some params => (toJson (message none)).setObjVal! "params" params
  json.compress

private def notification (method : String) (params? : Option Json) : String :=
  serializeCall (JsonRpc.Message.notification method) params?

private partial def writeLoop (input : IO.FS.Stream) (state : Std.Mutex State) : EIO ClientError Unit := do
  try
    repeat
      let (frame?, stopped) ← state.atomically do
        let s ← get
        if s.stopWriter && s.queue.isEmpty then return (none, true)
        let some frame := s.queue[0]? | return (none, false)
        set { s with
          queue := s.queue.extract 1 s.queue.size
          queuedBytes := s.queuedBytes - frame.payload.utf8ByteSize }
        -- Cancelled requests that have not started writing never reach the peer.
        if frame.requestId?.any (fun id => !s.pending.contains id) then return (none, false)
        modify fun s => { s with active? := some frame }
        return (some frame, false)
      if stopped then break
      if let some frame := frame? then
        input.writeSerializedLspMessage frame.payload
        state.atomically do modify fun s => { s with active? := none }
      else IO.sleep 2
  catch error => state.atomically do
    modify fun s => { (fail s error) with active? := none }

/-- Return whether the termination helper is known to have stopped. The watchdog
retains the helper while it is live, even if close runs out of time waiting for it. -/
private def forceTerminate {cfg : IO.Process.StdioConfig} (child : IO.Process.Child cfg)
    (errors : IO.Ref (Array String)) : BaseIO Bool := do
  if System.Platform.isWindows then
    if let .error error ← child.kill.toBaseIO then
      errors.modify (·.push s!"force termination failed: {error}")
    return true
  -- `takeStdin` rebuilds the child handle without the Unix `setsid` flag.
  let result ← (IO.Process.spawn {
    cmd := "kill", args := #["-KILL", "--", s!"-{child.pid}"]
    stdin := .null, stdout := .null, stderr := .null }).toBaseIO
  let .ok killer := result | do
    if let .error error := result then errors.modify (·.push s!"force termination failed: {error}")
    return true
  let deadline := (← IO.monoMsNow) + 500
  let mut stopping := false
  repeat
    match ← killer.tryWait.toBaseIO with
    | .ok (some code) =>
      if code != 0 && !stopping then
        errors.modify (·.push s!"force-termination helper exited with status {code}")
      return true
    | .ok none | .error (.interrupted ..) => pure ()
    | .error error =>
      -- Without an exit status we cannot prove the helper has stopped. Do
      -- not signal its PID or let close release the session's group ID.
      errors.modify (·.push s!"could not wait for force-termination helper: {error}")
      return false
    if !stopping && (← IO.monoMsNow) ≥ deadline then
      errors.modify (·.push "force-termination helper did not exit")
      if let .error error ← killer.kill.toBaseIO then
        errors.modify (·.push s!"could not stop force-termination helper: {error}")
      stopping := true
    IO.sleep (if stopping then 50 else 2)
  return false

/-- A partially written frame cannot be cancelled and followed by another frame.
The watchdog retires the transport and owns any group signal, including the
forced-close fallback. `finish` resolves to true when close requires that signal. -/
private def watchWrites {cfg : IO.Process.StdioConfig} (child : IO.Process.Child cfg)
    (state : Std.Mutex State) (finish : Task (Option Bool))
    (errors : IO.Ref (Array String)) : BaseIO Bool := do
  repeat
    let now ← IO.monoMsNow
    let (expired, stopped) ← state.atomically do
      let s ← get
      -- Closing seals the autonomous signal path before the graceful reap.
      if s.closing then return (false, true)
      if s.active?.any (fun frame => s.failure?.isSome || now ≥ frame.deadline) then
        set { (fail s (.deadline "write")) with writeTerminationRequested := true }
        return (true, false)
      return (false, s.stopWriter && s.active?.isNone)
    if expired then
      unless ← forceTerminate child errors do return false
      if (← errors.get).isEmpty then return true
      -- The helper stopped, so close may retry without overlapping signals.
      break
    if stopped then break
    IO.sleep 2
  if (← IO.wait finish).getD false then return ← forceTerminate child errors
  return true

private def configurationResult (config : ClientConfig) (params : Json) : Json :=
  let items := (params.getObjValAs? (Array Json) "items").toOption.getD #[]
  .arr <| items.map fun item =>
    let configSection := (item.getObjValAs? String "section").toOption.getD ""
    (config.configuration.find? (·.1 == configSection)).map (·.2) |>.getD .null

private def dispatch (config : ClientConfig) (state : Std.Mutex State) (message : Json) : EIO ClientError Unit := do
  unless (message.getObjValAs? String "jsonrpc").toOption == some "2.0" do
    throw <| ClientError.protocol "invalid JSON-RPC version"
  if let .ok method := message.getObjValAs? String "method" then
    let params := (message.getObjVal? "params").toOption.getD .null
    if let .ok id := message.getObjVal? "id" then
      unless (fromJson? (α := String) id).isOk || (fromJson? (α := Int) id).isOk do
        throw <| ClientError.protocol "invalid server request id"
      let result? := match method with
        | "workspace/configuration" => some <| configurationResult config params
        | "workspace/workspaceFolders" => some <| toJson #[Json.mkObj [
            ("uri", toJson (System.Uri.pathToUri config.root : String)),
            ("name", toJson (config.root.fileName.getD "project"))]]
        | "window/showMessageRequest" => some Json.null
        | _ => none
      let reply := match result? with
        | some result => Json.mkObj [("jsonrpc", "2.0"), ("id", id), ("result", result)]
        | none => Json.mkObj [("jsonrpc", "2.0"), ("id", id),
            ("error", Json.mkObj [("code", toJson (-32601 : Int)),
              ("message", toJson s!"unsupported client method: {method}")])]
      let payload := reply.compress
      let deadline := (← IO.monoMsNow) + config.timeoutMs
      state.atomically do
        let s ← get
        unless s.stopWriter do set (← enqueue config s { payload, deadline })
    else if method == "textDocument/publishDiagnostics" then
      let params ← protocol <| fromJson? (α := Lsp.PublishDiagnosticsParams) params
      state.atomically do
        modify fun s => match s.documents[params.uri]? with
          | none => s
          | some doc => { s with documents := s.documents.insert params.uri (doc.publish params) }
    else state.atomically do
      if config.retainedNotificationMethods.contains method then
        modify fun s => { s with latestNotifications := s.latestNotifications.insert method params }
      modify fun s =>
        if s.notifications.size < 512 then { s with notifications := s.notifications.push message }
        else { s with
          notifications := (s.notifications.extract 1 s.notifications.size).push message
          droppedNotifications := s.droppedNotifications + 1 }
  else
    let id ← protocol <| message.getObjValAs? Nat "id"
    let result := message.getObjVal? "result"
    let error := message.getObjVal? "error"
    unless result.isOk != error.isOk do throw <| ClientError.protocol "response must contain exactly one result or error"
    let response ← if let .ok error := error then do
        let code ← protocol <| error.getObjValAs? Int "code"
        let message ← protocol <| error.getObjValAs? String "message"
        pure <| Except.error <| ClientError.server code message (error.getObjVal? "data").toOption
      else pure <| Except.ok (← protocol result)
    state.atomically do
      modify fun s =>
        if s.pending.contains id then { s with pending := s.pending.insert id (some response) }
        else s -- Late responses never answer another operation.

private partial def receiveLoop (config : ClientConfig) (output : IO.FS.Stream)
    (state : Std.Mutex State) : EIO ClientError Unit := do
  try
    repeat
      let raw ← Internal.readFrame output config.maxInboundBytes
      let message ← protocol (Json.parse raw)
      dispatch config state message
  catch error => state.atomically do modify (fail · error)

private partial def drainStderr (handle : IO.FS.Handle) (bytes : IO.Ref ByteArray) : IO Unit := do
  repeat
    let chunk ← handle.read 4096
    if chunk.isEmpty then break
    bytes.modify fun old =>
      let combined := old ++ chunk
      combined.extract (combined.size - min combined.size 65536) combined.size

def Client.capabilities (client : Client) : BaseIO Json := do
  return ((← client.initializeResult.get).getObjVal? "capabilities").toOption.getD .null

def Client.stderr (client : Client) : BaseIO String := do
  return (String.fromUTF8? (← client.stderrBytes.get)).getD "[stderr contains non-UTF-8 bytes]"

private def Client.notifyCore (client : Client) (method : String) (params? : Option Json)
    (internal : Bool := false) : EIO ClientError Unit := do
  let payload := notification method params?
  let deadline := (← IO.monoMsNow) + client.config.timeoutMs
  client.state.atomically do
    let s ← get
    admit s internal
    set (← enqueue client.config s { payload, deadline })

/-- Admission to the bounded writer queue, not an acknowledgement by the peer.
Document lifecycle notifications must use the document operations below. -/
def Client.notify (client : Client) (method : String) (params : Json) : EIO ClientError Unit := do
  if #["textDocument/didOpen", "textDocument/didChange", "textDocument/didClose"].contains method then
    throw <| ClientError.invalidInput "use document lifecycle operations"
  client.notifyCore method (some params)

private def Client.cancel (client : Client) (id : Nat) : EIO ClientError Unit := do
  let deadline := (← IO.monoMsNow) + client.config.timeoutMs
  let payload := notification "$/cancelRequest" (some (Json.mkObj [("id", toJson id)]))
  client.state.atomically do
    let s ← get
    unless s.pending.contains id do return ()
    modify fun s => { s with pending := s.pending.erase id }
    let s ← get
    if s.closing then return ()
    if s.active?.any (·.requestId? == some id) then
      set <| fail s .disconnected
      return () -- The tracked watchdog handles termination, never this caller.
    unless s.stopWriter do
      try set (← enqueue client.config s { payload, deadline })
      catch _ => set <| fail s .queueFull

private def cancellationRequested (cancel? : Option Std.CancellationToken) : IO Bool :=
  match cancel? with
  | none => pure false
  | some token => token.isCancelled

private def Client.requestCore (client : Client) (method : String) (params? : Option Json)
    (timeoutMs : Nat) (cancel? : Option Std.CancellationToken)
    (document? : Option (String × Int) := none) (internal : Bool := false) : EIO ClientError Json := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  if ← cancellationRequested cancel? then throw <| ClientError.cancelled method
  -- Reserve only the ID so encoding can run outside the state mutex.
  let id ← client.state.atomically do
    modifyGet fun s => (s.nextId, { s with nextId := s.nextId + 1 })
  let payload := serializeCall (JsonRpc.Message.request (.num (JsonNumber.fromNat id)) method) params?
  if ← cancellationRequested cancel? then throw <| ClientError.cancelled method
  client.state.atomically do
    if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline method
    let s ← get
    admit s internal
    checkDocument s document?
    if s.pending.size ≥ client.config.maxPendingRequests then
      throw ClientError.pendingFull
    let s ← enqueue client.config s { payload, requestId? := some id, deadline }
    set { s with pending := s.pending.insert id none }
  try
    repeat
      if ← cancellationRequested cancel? then throw <| ClientError.cancelled method
      if (← IO.monoMsNow) ≥ deadline then throw <| ClientError.deadline method
      let response? ← client.state.atomically do
        let s ← get
        admit s internal
        checkDocument s document?
        let response? := s.pending[id]?.join
        if response?.isSome then modify fun s => { s with pending := s.pending.erase id }
        return response?
      if let some response := response? then return ← EIO.ofExcept response
      IO.sleep 2
    throw <| ClientError.protocol "unreachable request loop"
  catch error =>
    client.cancel id
    throw error

/-- Workspace/session operation. Document operations require a versioned handle.
Return its result, or a structured server error retaining the code and data. -/
def Client.request (client : Client) (method : String) (params : Json)
    (timeoutMs : Nat := client.config.timeoutMs) (cancel? : Option Std.CancellationToken := none) : EIO ClientError Json := do
  if (params.getObjVal? "textDocument").isOk then
    throw <| ClientError.invalidInput "document requests require a document handle"
  client.requestCore method (some params) timeoutMs cancel?

/-- Reject mismatched URIs and stale handles at both admission and delivery.
The handle itself selects the owning client; it cannot be reused in another one. -/
def Document.request (document : Document) (method : String) (params : Json)
    (timeoutMs : Nat := document.client.config.timeoutMs) (cancel? : Option Std.CancellationToken := none) : EIO ClientError Json := do
  unless (params.getObjVal? "textDocument" >>= (·.getObjValAs? String "uri")).toOption == some document.uri do
    throw <| ClientError.invalidInput "request URI does not match document handle"
  if let .ok raw := params.getObjVal? "textDocument" >>= (·.getObjVal? "version") then
    let version ← EIO.ofExcept <| (fromJson? (α := Int) raw).mapError ClientError.invalidInput
    unless version == document.version do throw <| ClientError.invalidInput "request version does not match document handle"
  document.client.requestCore method (some params) timeoutMs cancel? (some (document.uri, document.version))

def Client.openDocument (client : Client) (uri languageId text : String) : EIO ClientError Document := do
  let deadline := (← IO.monoMsNow) + client.config.timeoutMs
  let version ← client.state.atomically do
    let s ← get
    admit s
    if s.documents.contains uri then throw <| ClientError.invalidInput "document is already open"
    let version := s.nextVersion
    let payload := notification "textDocument/didOpen" <| some <| Json.mkObj [
      ("textDocument", Json.mkObj [("uri", toJson uri), ("languageId", toJson languageId),
        ("version", toJson version), ("text", toJson text)])]
    let s ← enqueue client.config s { payload, deadline }
    set { s with nextVersion := version + 1, documents := s.documents.insert uri { languageId, version, text } }
    return version
  return { client, uri, version }

private def Document.snapshot (document : Document) : EIO ClientError OpenDocument :=
  document.client.state.atomically do
    let s ← get
    admit s
    checkedDocument s document.uri document.version

/-- Full replacement for full sync, a whole-document range for incremental sync.
The returned handle supersedes the old version atomically with queue admission. -/
def Document.change (document : Document) (text : String) : EIO ClientError Document := do
  let client := document.client
  let caps ← client.capabilities
  let sync := (caps.getObjVal? "textDocumentSync").toOption.getD .null
  let kind := (fromJson? (α := Nat) sync).toOption <|> (sync.getObjValAs? Nat "change").toOption
  unless kind == some 1 || kind == some 2 do throw <| ClientError.unsupported "document synchronization"
  let change ← if kind == some 1 then pure <| Json.mkObj [("text", toJson text)] else do
    let doc ← document.snapshot
    let range : Lsp.Range := { start := ⟨0, 0⟩, «end» := Internal.sourceEndPosition doc.text }
    pure <| Json.mkObj [("range", toJson range), ("text", toJson text)]
  let version ← client.state.atomically do
    let s ← get
    admit s
    let doc ← checkedDocument s document.uri document.version
    let version := s.nextVersion
    let payload := notification "textDocument/didChange" <| some <| Json.mkObj [
      ("textDocument", Json.mkObj [("uri", toJson document.uri), ("version", toJson version)]),
      ("contentChanges", toJson #[change])]
    let deadline := (← IO.monoMsNow) + client.config.timeoutMs
    let s ← enqueue client.config s { payload, deadline }
    set { s with
      nextVersion := version + 1
      documents := s.documents.insert document.uri { doc with version, text, diagnostics? := none } }
    return version
  return { document with version }

def Document.close (document : Document) : EIO ClientError Unit := do
  let client := document.client
  let deadline := (← IO.monoMsNow) + client.config.timeoutMs
  let payload := notification "textDocument/didClose" <| some <| Json.mkObj [
    ("textDocument", Json.mkObj [("uri", toJson document.uri)])]
  client.state.atomically do
    let s ← get
    admit s
    checkDocument s (some (document.uri, document.version))
    let s ← enqueue client.config s { payload, deadline }
    set { s with documents := s.documents.erase document.uri }

/-- The exact text belonging to this admitted document version. -/
def Document.text (document : Document) : EIO ClientError String := do
  return (← document.snapshot).text

/-- Borrow the currently admitted version, without changing ownership. -/
def Client.document? (client : Client) (uri : String) : EIO ClientError (Option Document) :=
  client.state.atomically do
    let s ← get
    admit s
    return s.documents[uri]?.map fun doc => { client, uri, version := doc.version }

private def Document.withRelease (document : Document) (release : EIO ClientError Unit)
    (action : Document → EIO ClientError α) : EIO ClientError α := do
  let result ← (action document).toBaseIO
  let released ← release.toBaseIO
  match result, released with
  | .ok value, .ok () => return value
  | .error error, .ok () => throw error
  | .ok _, .error cause => throw <| ClientError.documentCleanup document.uri cause none
  | .error error, .error cause => throw <| ClientError.documentCleanup document.uri cause (some error)

/-- Scope a borrowed handle with the expected language and optional text.
Release validates the same admitted version and leaves the overlay in place. -/
def Document.withBorrowed (document : Document) (languageId : String)
    (action : Document → EIO ClientError α) (text? : Option String := none) : EIO ClientError α := do
  let doc ← document.snapshot
  unless doc.languageId == languageId do
    throw <| ClientError.invalidInput "cannot change a borrowed document language"
  if text?.any (· != doc.text) then
    throw <| ClientError.invalidInput "cannot replace a borrowed document overlay"
  document.withRelease (discard document.snapshot) action

/-- Scope an owned document, or borrow an open overlay with identical language
and text. Both action and release failures are retained. -/
def Client.withDocument (client : Client) (uri languageId text : String)
    (action : Document → EIO ClientError α) : EIO ClientError α := do
  if let some document ← client.document? uri then
    return ← document.withBorrowed languageId action (some text)
  let document ← client.openDocument uri languageId text
  document.withRelease document.close action

def Document.diagnostics (document : Document) : EIO ClientError (Option DiagnosticSnapshot) := do
  return (← document.snapshot).diagnostics?

/-- Latest params for a configured general notification method, independent of
the journal. None means no retained value; some Json.null is a received value.
Like capabilities and stderr, this is a snapshot, not a session liveness test. -/
def Client.latestNotification (client : Client) (method : String) : BaseIO (Option Json) :=
  client.state.atomically do return (← get).latestNotifications[method]?

def Client.notifications (client : Client) : BaseIO (Array Json × Nat) :=
  client.state.atomically do return ((← get).notifications, (← get).droppedNotifications)

private def Client.workerStatus (client : Client) : BaseIO WorkerStatus := do
  return {
    reader := ← IO.hasFinished client.reader
    writer := ← IO.hasFinished client.writer
    stderr := ← IO.hasFinished client.stderrReader
    watchdog := ← IO.hasFinished client.watchdog }

private def waitFor (timeoutMs : Nat) (ready : BaseIO Bool) : BaseIO Bool := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if ← ready then return true
    if (← IO.monoMsNow) ≥ deadline then return false
    IO.sleep 2
  return false

private def Client.pipesStopped (client : Client) : BaseIO Bool := do
  let workers ← client.workerStatus
  return workers.reader && workers.writer && workers.stderr

private def Client.stopWatchdog (client : Client) : BaseIO Bool := do
  client.finishWatchdog.resolve false
  waitFor 100 (IO.hasFinished client.watchdog)

private def Client.reapChild (client : Client) (timeoutMs : Nat) : IO Bool := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    try
      if (← client.child.tryWait).isSome then return true
    catch error =>
      match error with
      | .interrupted .. => if (← IO.monoMsNow) ≥ deadline then throw error
      | _ => throw error
    if (← IO.monoMsNow) ≥ deadline then return false
    IO.sleep 2
  return false

private def processGroupStatus (stat : ByteArray) : IO (Nat × Bool) := do
  -- The process name may contain parentheses and arbitrary non-UTF-8 bytes.
  let mut lastParen := none
  for i in [:stat.size] do
    if stat[i]! == ')'.toUInt8 then lastParen := some i
  let some i := lastParen | throw <| IO.userError "invalid proc process status"
  let some fields := String.fromUTF8? (stat.extract (i + 1) stat.size)
    | throw <| IO.userError "invalid proc process fields"
  let fields := (fields.splitOn " ").filter (· != "")
  let (some status, some group, some threads) :=
    (fields[0]?, fields[2]?.bind String.toNat?, fields[17]?.bind String.toNat?)
    | throw <| IO.userError "invalid proc process group status"
  -- A zombie main thread can still have live siblings.
  return (group, (status == "Z" || status == "X") && threads ≤ 1)

/-- The unreaped leader reserves the group ID. Before a graceful reap, confirm
that the leader has exited before taking the proc directory snapshot. Any other
member or vanished entry could have forked since the snapshot. After SIGKILL,
check that no live member remains. -/
private def Client.groupQuiet (client : Client) (deadline : Nat) (graceful : Bool) : IO Bool := do
  let leader := client.child.pid.toNat
  if graceful then
    let (group, exited) ← processGroupStatus (← IO.FS.readBinFile s!"/proc/{leader}/stat")
    unless group == leader do throw <| IO.userError "child no longer owns its process group"
    unless exited do return false
  for entry in ← System.FilePath.readDir "/proc" do
    let some pid := entry.fileName.toNat? | continue
    if (← IO.monoMsNow) ≥ deadline then return false
    let stat? ← try
      pure <| some (← IO.FS.readBinFile (entry.path / "stat"))
    catch error =>
      match error with
      -- A proc entry can vanish before open (ENOENT) or between open and read (ESRCH).
      | .noFileOrDirectory .. | .noSuchThing _ 3 _ => pure none
      | _ => throw error
    let some stat := stat? | if graceful then return false else continue
    let (group, exited) ← processGroupStatus stat
    if group == leader && (!exited || graceful && pid != leader) then
      return false
  return true

private def Client.waitGroupQuiet (client : Client) (timeoutMs : Nat) (graceful : Bool) : IO Bool := do
  let deadline := (← IO.monoMsNow) + timeoutMs
  repeat
    if ← client.groupQuiet deadline graceful then return true
    if (← IO.monoMsNow) ≥ deadline then return false
    IO.sleep 2
  return false

private def Client.closeCore (client : Client) : IO CloseReport := do
  let uris ← client.state.atomically do
    let s ← get
    set { s with pending := {}, documents := {} }
    return s.documents.toList.map (·.1)
  for uri in uris do
    discard <| (client.notifyCore "textDocument/didClose" (some (Json.mkObj [
      ("textDocument", Json.mkObj [("uri", toJson uri)])])) true).toBaseIO
  discard <| (client.requestCore "shutdown" none 500 none none true).toBaseIO
  discard <| (client.notifyCore "exit" none true).toBaseIO
  client.state.atomically do modify fun s => { s with stopWriter := true }
  let mut errors := #[]
  let writeTerminationRequested ← client.state.atomically do return (← get).writeTerminationRequested
  -- Descendants can redirect all inherited stdio. On Linux, pipe EOF must be
  -- followed by a group check before reaping the leader.
  let pipesStopped ← if writeTerminationRequested then pure false else waitFor 500 client.pipesStopped
  if pipesStopped then
    let groupQuiet ← if System.Platform.isLinux then
      try client.waitGroupQuiet 100 true
      catch error => do
        errors := errors.push s!"process-group inspection failed: {error}"
        pure false
    else pure true
    if groupQuiet then
      if System.Platform.isLinux then
        -- Procfs confirmed the leader's exit. Join the watchdog before reap.
        unless ← client.stopWatchdog do
          return {
            workers := ← client.workerStatus
            errors := errors.push "termination watchdog did not stop; child not reaped" }
      match ← (client.reapChild 100).toBaseIO with
      | .ok false =>
        if System.Platform.isLinux then
          return { workers := ← client.workerStatus, errors := errors.push "could not reap exited process-group leader" }
      | result =>
        unless System.Platform.isLinux do discard client.stopWatchdog
        if let .error error := result then errors := errors.push s!"could not reap child: {error}"
        return { reaped := result.isOk, workers := ← client.workerStatus, errors }
  client.finishWatchdog.resolve true
  -- Keep the group ID reserved until its only signal sender and its helper stop.
  let terminationStopped ← if ← waitFor 600 (IO.hasFinished client.watchdog) then
    IO.wait client.watchdog
  else pure false
  errors := errors ++ (← client.terminationErrors.get)
  unless terminationStopped do
    return {
      workers := ← client.workerStatus, forced := true
      errors := errors.push "termination may still be in progress; child not reaped" }
  if System.Platform.isLinux then
    try
      unless ← client.waitGroupQuiet 500 false do
        errors := errors.push "could not confirm the owned process group stopped"
    catch error => errors := errors.push s!"process-group inspection failed: {error}"
  discard <| waitFor 500 client.pipesStopped
  let result ← (client.reapChild 500).toBaseIO
  if let .error error := result then errors := errors.push s!"could not reap child: {error}"
  return { reaped := result.toOption.getD false, workers := ← client.workerStatus, forced := true, errors }

/-- One caller owns cleanup; concurrent and repeated callers share its report.
Closing rejects new work and invalidates all document handles immediately. -/
def Client.close (client : Client) : BaseIO CloseReport := do
  let owner ← client.state.atomically do
    let s ← get
    if s.closing then return false
    set { s with closing := true }
    return true
  if !owner then return (← IO.wait client.cleanup.result?).getD {}
  let report ← client.closeCore.catchExceptions fun error => pure { errors := #[error.toString] }
  client.cleanup.resolve report
  return report

def Client.start (config : ClientConfig) : EIO ClientError Client := do
  let root ← IO.FS.realPath config.root
  let config := { config with root }
  let (inputHandle, child) ← (← IO.Process.spawn {
    toStdioConfig := stdio, cmd := config.command, args := config.args,
    cwd := some config.root, env := config.env, setsid := true }).takeStdin
  let input := IO.FS.Stream.ofHandle inputHandle
  let state ← Std.Mutex.new ({} : State)
  let stderrBytes ← IO.mkRef ByteArray.empty
  let stderrReader ← IO.asTask (drainStderr child.stderr stderrBytes) .dedicated
  let reader ← EIO.asTask
    (receiveLoop config (IO.FS.Stream.ofHandle child.stdout) state) .dedicated
  let writer ← EIO.asTask (writeLoop input state) .dedicated
  let finishWatchdog ← IO.Promise.new
  let terminationErrors ← IO.mkRef #[]
  let watchdog ← BaseIO.asTask (watchWrites child state finishWatchdog.result? terminationErrors) .dedicated
  let cleanup ← IO.Promise.new
  let initializeResult ← IO.mkRef Json.null
  let client : Client := {
    child, state, reader, writer, watchdog, finishWatchdog, terminationErrors,
    stderrReader, stderrBytes, initializeResult, config, cleanup }
  try
    let rootUri := (System.Uri.pathToUri config.root : String)
    let result ← client.request "initialize" <| Json.mkObj [
      ("processId", toJson (← IO.Process.getPID).toNat), ("clientInfo", Json.mkObj [("name", "Rosetta")]),
      ("rootUri", toJson rootUri),
      ("workspaceFolders", toJson #[Json.mkObj [("uri", toJson rootUri), ("name", "project")]]),
      ("initializationOptions", config.initializationOptions),
      ("capabilities", Json.mkObj [
        ("experimental", config.experimentalCapabilities),
        ("general", Json.mkObj [("positionEncodings", toJson #["utf-16"])]),
        ("workspace", Json.mkObj [("configuration", toJson true), ("workspaceFolders", toJson true),
          ("symbol", Json.mkObj [("resolveSupport", Json.mkObj [
            ("properties", toJson #["location.range"])])])]),
        ("textDocument", Json.mkObj [
          ("publishDiagnostics", Json.mkObj [("versionSupport", toJson true)]),
          ("definition", Json.mkObj [("linkSupport", toJson true)]),
          ("diagnostic", Json.mkObj [("relatedDocumentSupport", toJson false)]),
          ("semanticTokens", Json.mkObj [
            ("requests", Json.mkObj [("full", toJson true), ("range", toJson true)]),
            ("tokenTypes", toJson #["namespace", "type", "class", "enum", "interface", "struct",
              "typeParameter", "parameter", "variable", "property", "enumMember", "event",
              "function", "method", "macro", "keyword", "modifier", "comment", "string",
              "number", "regexp", "operator", "decorator"]),
            ("tokenModifiers", toJson #["declaration", "definition", "readonly", "static",
              "deprecated", "abstract", "async", "modification", "documentation", "defaultLibrary"]),
            ("formats", toJson #["relative"]), ("overlappingTokenSupport", toJson false),
            ("multilineTokenSupport", toJson false)]),
          ("hover", Json.mkObj [("contentFormat", toJson #["plaintext", "markdown"])])])])]
    initializeResult.set result
    let caps ← client.capabilities
    let encoding := (caps.getObjValAs? String "positionEncoding").toOption.getD "utf-16"
    unless encoding == "utf-16" do throw <| ClientError.unsupported s!"position encoding: {encoding}"
    client.notify "initialized" (Json.mkObj [])
    return client
  catch error =>
    let report ← client.close
    if report.complete then throw error
    throw <| ClientError.cleanup report (some error)

/-- Preserve the action's failure if cleanup also fails. -/
def Client.withSession (config : ClientConfig) (action : Client → EIO ClientError α) : EIO ClientError α := do
  let client ← Client.start config
  let result : Except ClientError α ← try pure (.ok (← action client)) catch error => pure (.error error)
  let report ← client.close
  match result with
  | .ok value =>
    unless report.complete do throw <| ClientError.cleanup report none
    return value
  | .error error =>
    if report.complete then throw error
    throw <| ClientError.cleanup report (some error)

end Rosetta
