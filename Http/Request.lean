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

import Http.Headers
import Http.URI
import Http.Version

namespace Http

inductive Method
  | GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH

namespace Method

def toString : Method → String
  | GET     => "GET"
  | HEAD    => "HEAD"
  | POST    => "POST"
  | PUT     => "PUT"
  | DELETE  => "DELETE"
  | CONNECT => "CONNECT"
  | OPTIONS => "OPTIONS"
  | TRACE   => "TRACE"
  | PATCH   => "PATCH"

instance : ToString Method := ⟨toString⟩

end Method

structure Request (T) where
  method : Method
  url : URI
  version : Version
  headers : Headers
  body : T

namespace Request

/- TODO: use ToBytes typeclass or similar. -/
def toRequestString [ToString T] (r : Request T) : String :=
  s!"{r.method} {r.url.path} {r.version}" ++ CRLF ++
  r.headers.toRequestFormat ++
  CRLF ++ CRLF ++
  toString r.body
