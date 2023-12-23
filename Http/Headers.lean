/-  Copyright (C) 2023 The Http library contributors

    Authors: James Gallicchio
-/

import Std
import Http.Parser
import Http.HeaderName

open Parser Char
open Http.Parser

namespace Http

/-
Meta information for Requests and Responses.

TODO(urgent): Generalize to `ByteArray`, since lots of data is not valid UTF-8.
-/
def Headers := Std.HashMap HeaderName (List String)

namespace Headers

def toRequestFormat (h : Headers) : String :=
  h.fold (λ acc a b =>
    b.foldl (λ acc b => acc ++ s!"{a.toHeaderString}: {b}" ++ CRLF) acc) ""

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
  let value ← captureStr <| Parser.dropMany <| Parser.tokenFilter (fun c => c != '\n')
  ws
  return (key, value.2.toString)

def parse : Parser Headers := do
  let headers ← Array.foldl (λ map (k, v) => map.add k v) .empty <$>
    (Parser.takeMany parseHeader)
  ws
  return headers
