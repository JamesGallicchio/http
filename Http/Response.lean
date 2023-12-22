/-  Copyright (C) 2023 The Http library contributors

    Authors: James Gallicchio
-/

import Http.Status
import Http.Version
import Http.Headers
import Http.Parser

namespace Http

/-- A response to an HTTP request. -/
structure Response (T) where
  version : Version
  status : StatusCode
  headers : Headers
  body : T

end Http open Parser namespace Http.Response
open Http.Parser

def toString [ToString T] (r : Response T) : String :=
  s!"{r.version} {r.status.val}" ++
    (r.status.canonicalReason.map (" " ++ ·) |>.getD "") ++
    CRLF ++
    r.headers.toRequestFormat ++
    CRLF ++ CRLF ++
    ToString.toString r.body

def parse (p : Parser T) : Parser (Response T) := do
  let version ← Version.parse
  dropMany1 (tokenFilter (·.isWhitespace))
  let status ← StatusCode.parse
  let _ ← dropUntil (p := anyToken) (stop := token '\n')
  let headers ← Headers.parse
  let body ← p
  return {
    version
    status
    headers
    body := body
  }
