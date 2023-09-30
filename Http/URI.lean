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

def Path := String
deriving Inhabited, ToString

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

nonrec def toString : URI → String
| {scheme, auth, path, query, fragment} =>
  (scheme.map (toString · ++ "://") |>.getD "")
  ++ (auth.map toString |>.getD "")
  ++ toString path
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
    (token ':' *> some <$> (capture <| dropMany <| tokenFilter (·.isAlphanum)))
    <|> pure none
  let _ ← token '@'
  return ⟨user.toString, pword.map (·.toString)⟩

def Authority.Hostname.parse : Parser Authority.Hostname := do
  let str ← capture <| dropMany <| tokenFilter (fun c => c.isAlphanum || c = '-' || c = '.')
  return str.toString

def Authority.Port.parse : Parser Authority.Port := do
  let _ ← token ':'
  let digs ← capture <| dropMany <| tokenFilter (·.isDigit)
  let num := digs.toString.toNat!
  if h:num < UInt16.size then
    return ⟨num, h⟩
  else
    throwUnexpectedWithMessage none s!"port does not fit in a uint16: {num}"

def Authority.parse : Parser Authority := do
  let ui ← option? UserInfo.parse
  let host ← Hostname.parse
  let port ← option? Port.parse
  return { ui, host, port }

def Path.parse : Parser Path := do
  let str ← capture <| dropMany <|
    tokenFilter (fun c => c != '?' && c != '#')
  return str.toString

def Query.parse : Parser Query := do
  let _ ← token '?'
  let str ← capture <| dropMany <| tokenFilter (fun c => c != '#')
  return str.toString 

def Fragment.parse : Parser Fragment := do
  let _ ← token '#'
  let str ← capture <| dropMany anyToken
  return str.toString

def parse : Parser URI := do  
  let scheme ← option? do
    let res ← Scheme.parse
    let _ ← tokenArray #[':', '/', '/']
    pure res
  let auth ← option? Authority.parse
  let path ← Path.parse
  let query ← option? Query.parse
  let fragment ← option? Fragment.parse
  return { scheme, auth, path, query, fragment }
