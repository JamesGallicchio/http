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

import Std
import Http.Parser
import Http.HeaderName

open Http.Parser

namespace Http

/-
Meta information for Requests and Responses.

TODO(urgent): Generalize to `ByteArray`, since lots of data is not valid UTF-8.
-/
def Headers := Std.HashMap HeaderName (List String)

namespace Headers

def toRequestFormat (h : Headers) : String :=
  h.fold (λ acc a b => acc ++ s!"{a.toHeaderString}: {b}" ++ CRLF) ""

def empty : Headers := .empty

def add (self : Headers) (name : HeaderName) (value : String) : Headers :=
  match self.find? name with
  | none => self.insert name [value]
  | some v => self.insert name (value :: v)

def merge (a b : Headers) : Headers := a.mergeWith (fun _ => (· ++ ·)) b

def fromList (l : List (HeaderName × String)) : Headers :=
  l.foldl (λ h (n, v) => h.add n v) Std.HashMap.empty

def parseHeader : Parser (HeaderName × String) := do
  let key ← HeaderName.parse
  ws
  let _ ← Parser.token ':'
  ws  
  let value ← capture <| Parser.dropMany <| Parser.tokenFilter (fun c => c != '\n')
  ws
  return (key, value.toString)

def parse : Parser Headers := do
  let headers ← Array.foldl (λ map (k, v) => map.add k v) .empty <$>
    (Parser.takeMany parseHeader)
  ws
  return headers
