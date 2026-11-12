# Migration to the standalone repository

Rosetta now has one source repository, `ejgallego/rosetta`, containing the root
`rosetta` package and the nested `verso-rosetta` package. Lean imports remain
`Rosetta` and `VersoRosetta`. Package configuration examples are in the root
and bridge READMEs. Pin an exact standalone commit and remove the former
vendored/path dependency; avoid mixing copies of the same package.

## Earlier VBP snapshots

The implementation was exported from VBP `bc6c4f5af69ae8c174b93a73e68a781c0e68e073`.
The exported core and bridge Lean sources are byte-for-byte identical to that
snapshot. The four commits since the first package extraction retain their
history and authorship, with paths relocated to this repository's root.
Earlier development remains in VBP history. See [origin](ORIGIN.md).

VBP owns its Blueprint-specific generator, offline rendering tests and browser
fixtures. This repository owns the protocol/lifecycle tests, mock peer, real
server fixture sources, and the pure Verso bridge collection smoke test.

## Vendored lean-mirror readiness patch

The patch at lean-mirror `8d1e2e7` exposed a Rust-specific `Client.serverStatus`.
Standalone Rosetta adopts the behavior through generic notification retention:

```lean
-- Configure before Client.start / Client.withSession:
retainedNotificationMethods := #["experimental/serverStatus"]
-- Replace client.serverStatus with:
client.latestNotification "experimental/serverStatus"
```

Keep `experimentalCapabilities.serverStatusNotification = true` in the Rust
facade, and keep its `quiescent` predicate. The getter returns `BaseIO (Option
Json)`. `none` means no retained value; `some Json.null` is a received null value.
One latest value per configured method survives journal eviction, including
messages received during initialization. Diagnostics remain on document handles.

`Client.waitForNotification` now requires the method to be configured for
retention before startup; otherwise it returns `ClientError.invalidInput`.
The generic core contains no Rust-specific status field. Readiness is a
server-specific observation, not a liveness check or a promise against later
indexing work. See [API](API.md) for the complete contract.
