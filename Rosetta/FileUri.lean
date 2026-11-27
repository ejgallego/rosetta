/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: Emilio J. Gallego Arias
-/
module

import Std.Http.Data.URI.Encoding
import Init.System.Uri

namespace Rosetta

/-- Decode a local file URI for filesystem access. Other authorities, query
strings and fragments identify distinct LSP resources. -/
public def localFileUriPath? (uri : String) : Option System.FilePath := do
  guard ((uri.take 5).copy.toLower == "file:")
  let rest := uri.drop 5
  let path ← if let some rest := rest.dropPrefix? "//" then do
      let authority := rest.takeWhile (· != '/')
      guard (authority.isEmpty || authority.copy.toLower == "localhost")
      pure (rest.drop authority.utf8ByteSize)
    else pure rest
  guard (path.startsWith '/' && !(path.contains '?' || path.contains '#'))
  let encoded ← Std.Http.URI.EncodedString.ofString? (r := fun _ => true) path.copy
  let decoded ← encoded.decode
  guard (!decoded.contains '\x00')
  if !System.Platform.isWindows then return ⟨decoded⟩
  -- Reuse Lean's Windows drive-letter and separator handling after validation.
  let file ← System.Uri.fileUriToPath? ("file://" ++ path.copy)
  guard (!file.toString.startsWith "\\\\")
  return file

end Rosetta
