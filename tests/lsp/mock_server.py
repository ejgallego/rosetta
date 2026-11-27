"""Deterministic stdio peer for the experimental Lean client. No dependencies."""

import json
import os
import re
import signal
import sys
import threading
import time


def process_fields(pid):
    with open(f"/proc/{pid}/stat", "rb") as status:
        return status.read().rsplit(b")", 1)[1].decode("ascii").split()


def descendant_running(pid, group, started):
    try:
        fields = process_fields(pid)
    except (FileNotFoundError, ProcessLookupError):
        return False
    return fields[2] == group and fields[19] == started and fields[0] not in ("Z", "X")


if len(sys.argv) > 1 and sys.argv[1] == "--check-descendant":
    _, _, pid_arg, group, started, timeout_ms = sys.argv
    pid = int(pid_arg)
    deadline = time.monotonic() + int(timeout_ms) / 1000
    while descendant_running(pid, group, started):
        if time.monotonic() >= deadline:
            raise SystemExit("detached-stdio descendant is still running")
        time.sleep(0.01)
    sys.exit(0)


write_lock = threading.Lock()
chatter = {}
held = None
client_request = None
document_request = None
hold_next_token = False
cancelled_requests = []
mode = None
shutdown_seen = False
source_uri = None
target_uri = None
token_mode = None
expected_token_range = None
document_opens = 0
definition_requests = 0
documents = {}


def send(message):
    data = json.dumps({"jsonrpc": "2.0", **message}, ensure_ascii=False).encode()
    with write_lock:
        sys.stdout.buffer.write(f"Content-Length: {len(data)}\r\n\r\n".encode() + data)
        sys.stdout.buffer.flush()


def reply(request_id, result):
    send({"id": request_id, "result": result})


def workspace_symbol(name, container, *, resolve=None):
    location = {"uri": source_uri}
    symbol = {"name": name, "kind": 12, "location": location}
    if container is not None:
        symbol["containerName"] = container
    if resolve is None:
        location["range"] = {"start": {"line": 0, "character": 10 if container == "Other" else 9},
                             "end": {"line": 0, "character": 26}}
    else:
        symbol["data"] = resolve
    return symbol


def diagnostics(uri, version, message=None):
    items = [] if message is None else [{
        "range": {"start": {"line": 0, "character": 0},
                  "end": {"line": 0, "character": 1}},
        "severity": 2, "message": message,
    }]
    send({"method": "textDocument/publishDiagnostics", "params": {
        "uri": uri, "version": version, "diagnostics": items,
    }})


def chat(request_id, stopped):
    while not stopped.wait(0.003):
        send({"method": "window/logMessage", "params": {"type": 3, "message": "busy"}})
    # A late response must never answer the next operation.
    reply(request_id, "late")


def utf16_offset(text, position):
    breaks = list(re.finditer(r"\r\n|\r|\n", text))
    line = position["line"]
    assert 0 <= line <= len(breaks)
    start = breaks[line - 1].end() if line else 0
    end = breaks[line].start() if line < len(breaks) else len(text)
    prefix = text[start:end].encode("utf-16-le")[:2 * position["character"]]
    return start + len(prefix.decode("utf-16-le"))


while True:
    headers = {}
    while True:
        line = sys.stdin.buffer.readline()
        if not line:
            # These adversarial peers also ignore EOF. The wait-eof peer
            # deliberately exits here to exercise exclusive stdin ownership.
            if mode in ("ignore-shutdown", "ignore-term"):
                while True:
                    time.sleep(60)
            sys.exit(0)
        if line == b"\r\n":
            break
        key, value = line.decode().split(":", 1)
        headers[key.lower()] = value.strip()
    message = json.loads(sys.stdin.buffer.read(int(headers["content-length"])))
    method = message.get("method")
    request_id = message.get("id")
    params = message.get("params", {})
    if method == "initialize":
        options = params.get("initializationOptions") or {}
        mode = options.get("mode")
        source_uri = options.get("sourceUri")
        target_uri = options.get("targetUri")
        token_mode = options.get("tokenMode")
        expected_token_range = options.get("expectedTokenRange")
        if mode == "workspace":
            assert params["capabilities"]["textDocument"]["semanticTokens"]["requests"]["range"] is True
            send({"method": "test/status", "params": {"ready": False}})
        if mode == "ignore-term":
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
        capabilities = {
            "positionEncoding": "utf-8" if mode == "wrong-encoding" else "utf-16",
            "textDocumentSync": 2 if mode == "incremental" else 1,
            "hoverProvider": mode != "local-definition",
        }
        definition_provider = options.get("definitionProvider", True)
        if definition_provider is not None:
            capabilities["definitionProvider"] = definition_provider
        if mode == "workspace":
            capabilities["workspaceSymbolProvider"] = {"resolveProvider": True}
            if token_mode != "absent":
                capabilities["semanticTokensProvider"] = {
                    "full": token_mode not in ("range", "none"),
                    "range": token_mode not in ("full", "none"),
                    "legend": {
                        "tokenTypes": ["type", "function"],
                        "tokenModifiers": ["declaration"],
                    },
                }
        reply(request_id, {"capabilities": capabilities})
    elif method == "workspace/symbol":
        query = params["query"]
        if query == "measure":
            reply(request_id, [workspace_symbol("measure", "Container"), workspace_symbol("measure", "Container"),
                               workspace_symbol("measure", "Other"), workspace_symbol("measureMore", "Container")])
        elif query == "deferred":
            reply(request_id, [workspace_symbol("deferredMore", "Container", resolve="server-error"),
                               workspace_symbol("deferredOther", 1, resolve="server-error"),
                               workspace_symbol("deferred", "Other", resolve="protocol"),
                               workspace_symbol("deferred", "Container", resolve="range")])
        elif query == "deferredMore":
            reply(request_id, [workspace_symbol(query, "Container", resolve="server-error")])
        elif query == "raw-defer":
            reply(request_id, [workspace_symbol("raw-deferred", None, resolve="range")])
        elif query == " literal::measure ":
            reply(request_id, [workspace_symbol(query, " Parent::Container ")])
        elif query == "":
            reply(request_id, [workspace_symbol(query, "Container")])
        elif query == "empty-container":
            reply(request_id, [workspace_symbol(query, "")])
        elif query == "reversed-location":
            symbol = workspace_symbol(query, "Container")
            location = symbol["location"]
            position_range = location["range"]
            position_range["start"], position_range["end"] = position_range["end"], position_range["start"]
            reply(request_id, [symbol])
        elif query in ("change-name", "change-kind", "change-container", "change-uri", "reverse-range"):
            reply(request_id, [workspace_symbol(query, "Container", resolve=query)])
        elif query == "complete-container":
            reply(request_id, [workspace_symbol(query, None, resolve=query)])
        elif query == "bad-name":
            reply(request_id, [workspace_symbol(1, "Container"), workspace_symbol(query, "Container")])
        elif query == "bad-container":
            reply(request_id, [workspace_symbol(query, 1), workspace_symbol(query, "Container")])
        elif query in ("bad-deferred-kind", "bad-deferred-container", "bad-deferred-tags"):
            symbol = workspace_symbol(query, "Container", resolve="server-error")
            if query == "bad-deferred-kind":
                del symbol["kind"]
            elif query == "bad-deferred-container":
                symbol["containerName"] = 1
            else:
                symbol["tags"] = ["invalid"]
            reply(request_id, [symbol])
        elif query in ("bad-uri", "bad-range"):
            symbol = workspace_symbol(query, "Container", resolve="server-error")
            field = "uri" if query == "bad-uri" else "range"
            symbol["location"][field] = 1
            reply(request_id, [symbol])
        else:
            reply(request_id, [])
    elif method == "workspaceSymbol/resolve":
        if params["data"] == "server-error":
            send({"id": request_id, "error": {"code": -32097, "message": "symbol resolution failed"}})
            continue
        start = {"line": 0, "character": 0}
        end = {"line": 0, "character": 5}
        change = params["data"]
        position_range = {"start": start, "end": end}
        if change == "protocol":
            position_range = "invalid"
        elif change == "change-name":
            params["name"] += "Changed"
        elif change == "change-kind":
            params["kind"] = 5
        elif change in ("change-container", "complete-container"):
            params["containerName"] = "Other"
        elif change == "change-uri":
            params["location"]["uri"] = target_uri
        elif change == "reverse-range":
            position_range = {"start": end, "end": start}
        else:
            assert change == "range"
        params["location"]["range"] = position_range
        reply(request_id, params)
    elif method == "textDocument/definition" and mode == "local-definition":
        end = {1: 1, 2: 99}[params["position"]["line"]]
        start = {"line": 0, "character": 0}
        reply(request_id, [{"targetUri": params["textDocument"]["uri"],
                            "targetRange": {"start": start, "end": {"line": 1, "character": 0}},
                            "targetSelectionRange": {"start": start, "end": {"line": 0, "character": end}}}])
    elif method == "textDocument/definition" and mode == "workspace":
        definition_requests += 1
        assert params["textDocument"]["uri"] == source_uri, params
        if params["position"] == {"line": 0, "character": 9}:
            reply(request_id, [{"targetUri": target_uri,
                               "targetRange": {"start": {"line": 0, "character": 0},
                                               "end": {"line": 0, "character": 17}},
                               "targetSelectionRange": {"start": {"line": 0, "character": 7},
                                                        "end": {"line": 0, "character": 13}},
                               "originSelectionRange": {"start": {"line": 0, "character": 9},
                                                        "end": {"line": 0, "character": 15}}}])
        else:
            assert params["position"] == {"line": 0, "character": 16}, params
            reply(request_id, [{"uri": target_uri,
                               "range": {"start": {"line": line, "character": 7},
                                         "end": {"line": line, "character": 14}}}
                              for line in (1, 2)])
    elif method in ("textDocument/semanticTokens/full", "textDocument/semanticTokens/range"):
        if hold_next_token:
            hold_next_token = False
            document_request = request_id
            send({"method": "test/held-highlight", "params": {}})
            continue
        if token_mode == "disconnect":
            sys.exit(0)
        if token_mode == "server-error":
            send({"id": request_id, "error": {"code": -32601, "message": "feature unavailable"}})
            continue
        if token_mode == "protocol":
            reply(request_id, {"data": [0]})
            continue
        if token_mode == "deadline":
            continue
        assert token_mode not in ("absent", "none")
        assert token_mode != "range" or method.endswith("/range")
        assert token_mode != "full" or method.endswith("/full")
        if expected_token_range is not None:
            assert params["range"] == expected_token_range, params
        data = [] if params["textDocument"]["uri"] == target_uri else [0, 9, 6, 0, 0, 0, 7, 7, 1, 1]
        reply(request_id, None if token_mode == "null" else {"data": data})
    elif method == "textDocument/didOpen":
        document_opens += 1
        document = params["textDocument"]
        if mode == "incremental":
            documents[document["uri"]] = document["text"]
        diagnostics(document["uri"], document["version"], "old warning")
        diagnostics(document["uri"], document["version"])
        diagnostics(document["uri"], document["version"] - 1, "stale warning")
    elif method == "textDocument/didChange":
        document = params["textDocument"]
        if mode == "incremental":
            uri = document["uri"]
            text = documents[uri]
            for change in params["contentChanges"]:
                start = utf16_offset(text, change["range"]["start"])
                end = utf16_offset(text, change["range"]["end"])
                text = text[:start] + change["text"] + text[end:]
            documents[uri] = text
        diagnostics(document["uri"], 1, "stale after edit")
        diagnostics(document["uri"], document["version"], "current warning")
    elif method == "textDocument/didClose" and mode == "incremental":
        del documents[params["textDocument"]["uri"]]
    elif method == "test/document-text":
        reply(request_id, documents[params["textDocument"]["uri"]])
    elif method == "test/workspace-traffic":
        reply(request_id, {"documentOpens": document_opens, "definitionRequests": definition_requests})
    elif method == "test/readiness":
        send({"method": "test/status", "params": {"ready": False}})
        for _ in range(600):
            send({"method": "window/logMessage", "params": {"type": 3, "message": "indexing"}})
        send({"method": "test/status", "params": {"ready": True}})
        send({"method": "test/null-status", "params": None})
        for _ in range(600):
            send({"method": "window/logMessage", "params": {"type": 3, "message": "finished"}})
        reply(request_id, True)
    elif method == "test/not-ready":
        send({"method": "test/status", "params": {"ready": False}})
        for _ in range(600):
            send({"method": "window/logMessage", "params": {"type": 3, "message": "busy again"}})
        reply(request_id, True)
    elif method == "test/barrier":
        reply(request_id, True)
    elif method == "test/hold-document":
        document_request = request_id
        send({"method": "test/held-" + params["tag"], "params": {}})
    elif method == "test/hold-next-token":
        assert "params" in message and params is None, message
        hold_next_token = True
    elif method == "test/release-document":
        reply(document_request, "obsolete")
        document_request = None
        reply(request_id, True)
    elif method == "test/stop-reading":
        reply(request_id, True)
        if params == "observe-write":
            # Acknowledge the next frame as soon as it reaches the pipe, then
            # leave its oversized body unread so the client writer cannot finish.
            if not sys.stdin.buffer.read(1):
                break
            send({"method": "test/write-started"})
        while True:
            time.sleep(60)
    elif method == "test/stderr":
        sys.stderr.write("e" * 200000)
        sys.stderr.flush()
        reply(request_id, True)
    elif method in ("test/descendant", "test/escaped-descendant", "test/detached-descendant"):
        ready_read, ready_write = os.pipe()
        pid = os.fork()
        if pid == 0:
            os.close(ready_read)
            if method == "test/escaped-descendant":
                os.setsid()
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            if method == "test/detached-descendant":
                with open("/proc/self/comm", "wb") as comm:
                    comm.write(b"lsp)child\xff")
                with open(os.devnull, "r+b", buffering=0) as null:
                    for fd in (0, 1, 2):
                        os.dup2(null.fileno(), fd)
            os.write(ready_write, b"r")
            os.close(ready_write)
            if method == "test/detached-descendant":
                # Bound the lifetime even when cleanup under test is broken.
                time.sleep(30)
                os._exit(0)
            while True:
                time.sleep(60)
        os.close(ready_write)
        assert os.read(ready_read, 1) == b"r"
        os.close(ready_read)
        if method == "test/detached-descendant":
            fields = process_fields(pid)
            reply(request_id, {"pid": pid, "group": fields[2], "started": fields[19]})
            continue
        reply(request_id, pid)
        if method == "test/descendant":
            sys.exit(0)
    elif method == "test/bad-envelope":
        send({"id": request_id, "result": True, "error": {"code": -1, "message": "bad"}})
    elif method == "test/bad-error":
        send({"id": request_id, "error": {"code": "invalid", "message": "bad"}})
    elif method == "test/oversized":
        sys.stdout.buffer.write(b"Content-Length: 999999999\r\n\r\n")
        sys.stdout.buffer.flush()
    elif method == "test/partial":
        sys.stdout.buffer.write(b"Content-Length: 100\r\n\r\n{")
        sys.stdout.buffer.flush()
    elif method == "test/error":
        error = {"code": -32097, "message": "server-specific"}
        if isinstance(params, dict):
            error["code"] = params.get("code", error["code"])
            if "data" in params:
                error["data"] = params["data"]
        send({"id": request_id, "error": error})
    elif method == "test/null":
        assert "params" in message and params is None, message
        reply(request_id, None)
    elif method == "test/cancellations":
        reply(request_id, cancelled_requests)
    elif method == "test/first":
        held = request_id
        send({"method": "test/held", "params": {}})
    elif method == "test/second":
        reply(request_id, "second")
        reply(held, "first")
    elif method == "test/client-request":
        client_request = request_id
        send({"id": "server-unsupported", "method": "client/registerCapability", "params": {}})
    elif method is None and request_id == "server-unsupported":
        reply(client_request, message.get("error", {}).get("code") == -32601)
    elif method == "test/chatter":
        stopped = threading.Event()
        chatter[request_id] = stopped
        threading.Thread(target=chat, args=(request_id, stopped), daemon=True).start()
    elif method == "$/cancelRequest":
        cancelled_requests.append(params["id"])
        if params["id"] in chatter:
            chatter[params["id"]].set()
    elif method == "test/unversioned":
        send({"method": "textDocument/publishDiagnostics", "params": {
            "uri": params["uri"], "diagnostics": [],
        }})
        reply(request_id, True)
    elif method == "test/die":
        sys.stderr.write("intentional mock exit\n")
        sys.stderr.flush()
        sys.exit(7)
    elif method == "shutdown":
        if mode == "strict-shutdown":
            assert not shutdown_seen and "params" not in message and request_id is not None, message
            shutdown_seen = True
            print("shutdown", file=sys.stderr, flush=True)
        if mode not in ("ignore-shutdown", "ignore-term"):
            reply(request_id, None)
    elif method == "exit":
        if mode == "strict-shutdown":
            assert shutdown_seen and "params" not in message and "id" not in message, message
            print("exit", file=sys.stderr, flush=True)
        if mode not in ("ignore-shutdown", "ignore-term", "wait-eof"):
            sys.exit(0)
