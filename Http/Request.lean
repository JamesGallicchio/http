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

def parse : Parser Method := do
  match ← Parser.anyToken with
  | 'C' =>
    let _ ← Parser.token 'O'
    let _ ← Parser.token 'N'
    let _ ← Parser.token 'N'
    let _ ← Parser.token 'E'
    let _ ← Parser.token 'C'
    let _ ← Parser.token 'T'
    return .CONNECT
  | 'D' =>
    let _ ← Parser.token 'E'
    let _ ← Parser.token 'L'
    let _ ← Parser.token 'E'
    let _ ← Parser.token 'T'
    let _ ← Parser.token 'E'
    return .DELETE
  | 'G' =>
    let _ ← Parser.token 'E'
    let _ ← Parser.token 'T'
    return .GET
  | 'H' =>
    let _ ← Parser.token 'E'
    let _ ← Parser.token 'A'
    let _ ← Parser.token 'D'
    return .HEAD
  | 'O' =>
    let _ ← Parser.token 'P'
    let _ ← Parser.token 'T'
    let _ ← Parser.token 'I'
    let _ ← Parser.token 'O'
    let _ ← Parser.token 'N'
    let _ ← Parser.token 'S'
    return .OPTIONS
  | 'P' =>
    match ← Parser.anyToken with
    | 'A' =>
      let _ ← Parser.token 'T'
      let _ ← Parser.token 'C'
      let _ ← Parser.token 'H'
      return .PATCH
    | 'O' =>
      let _ ← Parser.token 'S'
      let _ ← Parser.token 'T'
      return .POST
    | 'U' =>
      let _ ← Parser.token 'T'
      return .PUT
    | _ => Parser.throwUnexpected
  | 'T' =>
    let _ ← Parser.token 'R'
    let _ ← Parser.token 'A'
    let _ ← Parser.token 'C'
    let _ ← Parser.token 'E'
    return .TRACE
  | _ => Parser.throwUnexpected

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

def parse (p : Parser T) : Parser (Request T) := do
  let method ← Method.parse
  Parser.ws
  let url ← URI.parse
  Parser.ws
  let version ← Version.parse
  Parser.ws
  let headers ← Headers.parse
  Parser.ws
  let body ← p
  return {method, url, version, headers, body}
