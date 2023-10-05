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

import Lean
import Std
import Http.Parser
import Http.CaseInsString

namespace Http

namespace URI

structure Scheme where
  val : CaseInsString
  hlen : val.length > 0
  hfirst : val.get 0 |>.isAlpha
deriving DecidableEq

namespace Scheme

def ofString (s : String) : Option Scheme :=
  let s := .ofString s
  if h : _ ∧ _ then
    some {
      val := s
      hlen := h.1
      hfirst := h.2
    }
  else
    none

instance : Inhabited Scheme := ⟨ofString "a" |>.get (by simp only)⟩

def HTTP := ofString "http" |>.get!
def HTTPS := ofString "https" |>.get!

instance : Inhabited Scheme := ⟨HTTP⟩
instance : ToString Scheme := ⟨(·.val.val)⟩
end Scheme

namespace Authority

/-- Example: http://user:pass@example.com

TODO: invariants about char set / pct-encoding
-/
structure UserInfo where
  username : String
  password : Option String
deriving Inhabited

instance : ToString UserInfo where
  toString ui := s!"{ui.username}" ++ (ui.password.map (s!":{·}") |>.getD "")

def Hostname := String
deriving Inhabited, ToString

def Port := UInt16
deriving DecidableEq, Inhabited, ToString

end Authority

open Authority in
structure Authority where
  ui : Option UserInfo
  host : Hostname
  port : Option Port
deriving Inhabited

instance : ToString Authority where
  toString | {ui,host,port} =>
              s!"{
                ui.map (toString · ++ "@") |>.getD ""
              }{host}{
                port.map (":" ++ toString ·) |>.getD ""
              }"

def Path := Array String
deriving Inhabited, ToString

instance : Append Path := inferInstanceAs <| Append (Array _)

def Query := String
deriving Inhabited, ToString

def Fragment := String
deriving Inhabited, ToString

end URI

open URI

structure URI where
  scheme : Option Scheme
  auth : Option Authority
  path : Path
  query : Option Query
  fragment : Option Fragment
deriving Inhabited

end Http open Parser
namespace Http

open Http.Parser

namespace URI

def appendPath (p : Path) (self : URI) : URI :=
  {self with path := self.path ++ p}

def withQuery (query : Query) (self : URI) : URI :=
  { self with query := some query }

def withFragment (fragment : Fragment) (self : URI) : URI :=
  { self with fragment := some fragment }

nonrec def toString : URI → String
| {scheme, auth, path, query, fragment} =>
  (scheme.map (toString · ++ "://") |>.getD "")
  ++ (auth.map toString |>.getD "")
  ++ (path.foldl (· ++ "/" ++ ·) "")
  ++ (query.map ("?" ++ toString ·) |>.getD "")
  ++ (fragment.map ("#" ++ toString ·) |>.getD "")

instance : ToString URI := ⟨toString⟩

/- TODO: These parsers are almost certainly not good enough.

E.g. percent-encoding is not taken into account.

Unfortunately the standard seems to be https://url.spec.whatwg.org/#urls
which takes hundreds of lines of text to explain how to parse
URLs.
-/

def Scheme.parse : Parser Scheme := do
  let str ← capture <| do
    let _ ← tokenFilter (·.isAlpha)
    let _ ← dropMany <| tokenFilter (fun c => c.isAlphanum || c ∈ ['+', '-', '.'])
  return Scheme.ofString str.toString |>.get!

def Authority.UserInfo.parse : Parser Authority.UserInfo := do
  let user : Substring ←
    capture <| dropMany <| tokenFilter (·.isAlphanum)
  let pword : Option Substring ←
    withBacktracking (token ':' *> some <$> (capture <| dropMany <| tokenFilter (·.isAlphanum)))
    <|> pure none
  let _ ← token '@'
  return ⟨user.toString, pword.map (·.toString)⟩

def Authority.Hostname.parse : Parser Authority.Hostname := do
  let str ← capture <| dropMany <| tokenFilter (fun c => c.isAlphanum || c = '-' || c = '.')
  return str.toString

def Authority.Port.parse : Parser Authority.Port := do
  let _ ← token ':'
  let digs ← capture <| dropMany <| tokenFilter (·.isDigit)
  match digs.toString.toNat? with
  | none =>
    throwUnexpectedWithMessage none s!"BUG: captured non-digit??: {digs}"
  | some num =>
  if h:num < UInt16.size then
    return ⟨num, h⟩
  else
    throwUnexpectedWithMessage none s!"port does not fit in a uint16: {num}"

def Authority.parse : Parser Authority := do
  let ui ← option? UserInfo.parse
  let host ← Hostname.parse
  let port ← option? Port.parse
  return { ui, host, port }

def Path.allowedSegChars : ByteArray :=
  .mk <| Array.ofFn (fun (i : Fin 256) =>
    let c : Char := Char.ofNat i
    if
      'A' ≤ c && c ≤ 'Z' ||
      'a' ≤ c && c ≤ 'z' ||
      '0' ≤ c && c ≤ '9' ||
      c ∈ ['-', '.', '_', '~', '!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=', ':', '@']
    then
      1
    else 0
  )

@[simp] theorem Path.allowedSegChars_size : Path.allowedSegChars.size = 256 := by
  simp [ByteArray.size, allowedSegChars]

def Path.pctEnc : Parser Char := do
  let _ ← token '%'
  let c1 ← tokenFilter Unicode.isHexDigit
  let c2 ← tokenFilter Unicode.isHexDigit
  let byte := (Unicode.getHexDigit? c1).get!.val * 16 + (Unicode.getHexDigit? c2).get!.val
  return Char.ofNat byte

def Path.parse : Parser Path := do
  let parts ← takeMany <|
    token '/' *> (capture <| dropMany <|
      (tokenFilter (fun c =>
        if h : c.toNat < 256 then
          have := by rw [←Path.allowedSegChars_size] at h; exact h
          Path.allowedSegChars[c.toNat] > 0
        else
          false
      )
      <|>
      Path.pctEnc)
    )
  return parts.map (·.toString)

def Query.parse : Parser Query := do
  let _ ← token '?'
  let str ← capture <| dropMany <| tokenFilter (fun c => c != '#')
  return str.toString 

def Fragment.parse : Parser Fragment := do
  let _ ← token '#'
  let str ← capture <| dropMany anyToken
  return str.toString

def parse : Parser URI := do  
  let scheme ← option? <| withBacktracking do
    let res ← Scheme.parse
    let _ ← tokenArray #[':', '/', '/']
    pure res
  let auth ← option? (withBacktracking Authority.parse)
  let path ← Path.parse
  let query ← option? Query.parse
  let fragment ← option? Fragment.parse
  return { scheme, auth, path, query, fragment }

def fromString? (s : String) : Option URI :=
  match parse.run s with
  | .ok ss u =>
    if ss.isEmpty then some u else none
  | _ => none

#eval fromString? "https://api.github.com" |>.map (·.appendPath #["hi"])
