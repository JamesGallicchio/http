/-  Copyright (C) 2023 The Http library contributors.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
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
