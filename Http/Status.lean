/-
    Copyright (C) 2023 the Http library contributors.

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
import Qq
import Http.Std
import Http.Parser

open Parser

/-! # HTTP Status Codes -/

namespace Http

/-- HTTP Status Code -/
def StatusCode := { c : UInt16 // 100 ≤ c ∧ c < 1000 }
deriving DecidableEq, Hashable

namespace StatusCode

/-- Check if status is in range 100-199. -/
def isInformational (c : StatusCode) := c.val < 200

/-- Check if status is in range 200-299. -/
def isSuccess (c : StatusCode) := 200 <= c.val && c.val < 300

/-- Check if status is in range 300-399. -/
def isRedirection (c : StatusCode) := 300 <= c.val && c.val < 400

/-- Check if status is in range 400-499. -/
def isClientError (c : StatusCode) := 400 <= c.val && c.val < 500

/-- Check if status is in range 500-599. -/
def isServerError (c : StatusCode) := 500 <= c.val && c.val < 600

section
open Lean Macro Elab Qq Command Meta Parser

local elab "genStatusCodes!" codes:term : command => do
  let expectedType := q(Array (Nat × Name × String × String))
  let list ← liftTermElabM do
    let elabList ← instantiateMVars <| ← Term.withSynthesize <|
          Term.elabTermEnsuringType codes (expectedType? := some expectedType)
    return ← unsafe evalExpr (Array (Nat × Name × String × String)) expectedType elabList

  for (num, id, name, ref) in list do
    elabCommand (← `(
      $(mkDocComment s!"{num} {name}\n\n{ref}"):docComment
      def $(mkIdent id) : StatusCode := ⟨$(Quote.quote num), by decide⟩
    ))
  
  let canonicalReasonId := mkIdent `canonicalReason
  
  elabCommand (← `(
    /-- The canonical interpretation of a given status code, as a string. -/
    def $canonicalReasonId (c : StatusCode) : Option String :=
      match c with
      $(← open Lean.Parser.Term in
        list.mapM fun (num,_,name,_) => `(matchAltExpr|
        | .mk $(Quote.quote num) _ => some $(quote name)
      )):matchAlt*
      | _ => none
  ))

/- list of status codes and references pulled from
[rust http package](https://docs.rs/http/latest/src/http/status.rs.html)
-/
genStatusCodes! #[
  (100, `CONTINUE                       , "Continue"                       , "[RFC7231, Section 6.2.1](https://tools.ietf.org/html/rfc7231#section-6.2.1)"),
  (101, `SWITCHING_PROTOCOLS            , "Switching Protocols"            , "[RFC7231, Section 6.2.2](https://tools.ietf.org/html/rfc7231#section-6.2.2)"),
  (102, `PROCESSING                     , "Processing"                     , "[RFC2518](https://tools.ietf.org/html/rfc2518)"),

  (200, `OK                             , "OK"                             , "[RFC7231, Section 6.3.1](https://tools.ietf.org/html/rfc7231#section-6.3.1)"),
  (201, `CREATED                        , "Created"                        , "[RFC7231, Section 6.3.2](https://tools.ietf.org/html/rfc7231#section-6.3.2)"),
  (202, `ACCEPTED                       , "Accepted"                       , "[RFC7231, Section 6.3.3](https://tools.ietf.org/html/rfc7231#section-6.3.3)"),
  (203, `NON_AUTHORITATIVE_INFORMATION  , "Non Authoritative Information"  , "[RFC7231, Section 6.3.4](https://tools.ietf.org/html/rfc7231#section-6.3.4)"),
  (204, `NO_CONTENT                     , "No Content"                     , "[RFC7231, Section 6.3.5](https://tools.ietf.org/html/rfc7231#section-6.3.5)"),
  (205, `RESET_CONTENT                  , "Reset Content"                  , "[RFC7231, Section 6.3.6](https://tools.ietf.org/html/rfc7231#section-6.3.6)"),
  (206, `PARTIAL_CONTENT                , "Partial Content"                , "[RFC7233, Section 4.1](https://tools.ietf.org/html/rfc7233#section-4.1)"),
  (207, `MULTI_STATUS                   , "Multi-Status"                   , "[RFC4918](https://tools.ietf.org/html/rfc4918)"),
  (208, `ALREADY_REPORTED               , "Already Reported"               , "[RFC5842](https://tools.ietf.org/html/rfc5842)"),

  (226, `IM_USED                        , "IM Used"                        , "[RFC3229](https://tools.ietf.org/html/rfc3229)"),

  (300, `MULTIPLE_CHOICES               , "Multiple Choices"               , "[RFC7231, Section 6.4.1](https://tools.ietf.org/html/rfc7231#section-6.4.1)"),
  (301, `MOVED_PERMANENTLY              , "Moved Permanently"              , "[RFC7231, Section 6.4.2](https://tools.ietf.org/html/rfc7231#section-6.4.2)"),
  (302, `FOUND                          , "Found"                          , "[RFC7231, Section 6.4.3](https://tools.ietf.org/html/rfc7231#section-6.4.3)"),
  (303, `SEE_OTHER                      , "See Other"                      , "[RFC7231, Section 6.4.4](https://tools.ietf.org/html/rfc7231#section-6.4.4)"),
  (304, `NOT_MODIFIED                   , "Not Modified"                   , "[RFC7232, Section 4.1](https://tools.ietf.org/html/rfc7232#section-4.1)"),
  (305, `USE_PROXY                      , "Use Proxy"                      , "[RFC7231, Section 6.4.5](https://tools.ietf.org/html/rfc7231#section-6.4.5)"),
  (307, `TEMPORARY_REDIRECT             , "Temporary Redirect"             , "[RFC7231, Section 6.4.7](https://tools.ietf.org/html/rfc7231#section-6.4.7)"),
  (308, `PERMANENT_REDIRECT             , "Permanent Redirect"             , "[RFC7238](https://tools.ietf.org/html/rfc7238)"),

  (400, `BAD_REQUEST                    , "Bad Request"                    , "[RFC7231, Section 6.5.1](https://tools.ietf.org/html/rfc7231#section-6.5.1)"),
  (401, `UNAUTHORIZED                   , "Unauthorized"                   , "[RFC7235, Section 3.1](https://tools.ietf.org/html/rfc7235#section-3.1)"),
  (402, `PAYMENT_REQUIRED               , "Payment Required"               , "[RFC7231, Section 6.5.2](https://tools.ietf.org/html/rfc7231#section-6.5.2)"),
  (403, `FORBIDDEN                      , "Forbidden"                      , "[RFC7231, Section 6.5.3](https://tools.ietf.org/html/rfc7231#section-6.5.3)"),
  (404, `NOT_FOUND                      , "Not Found"                      , "[RFC7231, Section 6.5.4](https://tools.ietf.org/html/rfc7231#section-6.5.4)"),
  (405, `METHOD_NOT_ALLOWED             , "Method Not Allowed"             , "[RFC7231, Section 6.5.5](https://tools.ietf.org/html/rfc7231#section-6.5.5)"),
  (406, `NOT_ACCEPTABLE                 , "Not Acceptable"                 , "[RFC7231, Section 6.5.6](https://tools.ietf.org/html/rfc7231#section-6.5.6)"),
  (407, `PROXY_AUTHENTICATION_REQUIRED  , "Proxy Authentication Required"  , "[RFC7235, Section 3.2](https://tools.ietf.org/html/rfc7235#section-3.2)"),
  (408, `REQUEST_TIMEOUT                , "Request Timeout"                , "[RFC7231, Section 6.5.7](https://tools.ietf.org/html/rfc7231#section-6.5.7)"),
  (409, `CONFLICT                       , "Conflict"                       , "[RFC7231, Section 6.5.8](https://tools.ietf.org/html/rfc7231#section-6.5.8)"),
  (410, `GONE                           , "Gone"                           , "[RFC7231, Section 6.5.9](https://tools.ietf.org/html/rfc7231#section-6.5.9)"),
  (411, `LENGTH_REQUIRED                , "Length Required"                , "[RFC7231, Section 6.5.10](https://tools.ietf.org/html/rfc7231#section-6.5.10)"),
  (412, `PRECONDITION_FAILED            , "Precondition Failed"            , "[RFC7232, Section 4.2](https://tools.ietf.org/html/rfc7232#section-4.2)"),
  (413, `PAYLOAD_TOO_LARGE              , "Payload Too Large"              , "[RFC7231, Section 6.5.11](https://tools.ietf.org/html/rfc7231#section-6.5.11)"),
  (414, `URI_TOO_LONG                   , "URI Too Long"                   , "[RFC7231, Section 6.5.12](https://tools.ietf.org/html/rfc7231#section-6.5.12)"),
  (415, `UNSUPPORTED_MEDIA_TYPE         , "Unsupported Media Type"         , "[RFC7231, Section 6.5.13](https://tools.ietf.org/html/rfc7231#section-6.5.13)"),
  (416, `RANGE_NOT_SATISFIABLE          , "Range Not Satisfiable"          , "[RFC7233, Section 4.4](https://tools.ietf.org/html/rfc7233#section-4.4)"),
  (417, `EXPECTATION_FAILED             , "Expectation Failed"             , "[RFC7231, Section 6.5.14](https://tools.ietf.org/html/rfc7231#section-6.5.14)"),
  (418, `IM_A_TEAPOT                    , "I'm a teapot"                   , "curiously not registered by IANA but [RFC2324](https://tools.ietf.org/html/rfc2324)"),

  (421, `MISDIRECTED_REQUEST            , "Misdirected Request"            , "[RFC7540, Section 9.1.2](http://tools.ietf.org/html/rfc7540#section-9.1.2)"),
  (422, `UNPROCESSABLE_ENTITY           , "Unprocessable Entity"           , "[RFC4918](https://tools.ietf.org/html/rfc4918)"),
  (423, `LOCKED                         , "Locked"                         , "[RFC4918](https://tools.ietf.org/html/rfc4918)"),
  (424, `FAILED_DEPENDENCY              , "Failed Dependency"              , "[RFC4918](https://tools.ietf.org/html/rfc4918)"),

  (426, `UPGRADE_REQUIRED               , "Upgrade Required"               , "[RFC7231, Section 6.5.15](https://tools.ietf.org/html/rfc7231#section-6.5.15)"),

  (428, `PRECONDITION_REQUIRED          , "Precondition Required"          , "[RFC6585](https://tools.ietf.org/html/rfc6585)"),
  (429, `TOO_MANY_REQUESTS              , "Too Many Requests"              , "[RFC6585](https://tools.ietf.org/html/rfc6585)"),

  (431, `REQUEST_HEADER_FIELDS_TOO_LARGE, "Request Header Fields Too Large", "[RFC6585](https://tools.ietf.org/html/rfc6585)"),

  (451, `UNAVAILABLE_FOR_LEGAL_REASONS  , "Unavailable For Legal Reasons"  , "[RFC7725](http://tools.ietf.org/html/rfc7725)"),

  (500, `INTERNAL_SERVER_ERROR          , "Internal Server Error"          , "[RFC7231, Section 6.6.1](https://tools.ietf.org/html/rfc7231#section-6.6.1)"),
  (501, `NOT_IMPLEMENTED                , "Not Implemented"                , "[RFC7231, Section 6.6.2](https://tools.ietf.org/html/rfc7231#section-6.6.2)"),
  (502, `BAD_GATEWAY                    , "Bad Gateway"                    , "[RFC7231, Section 6.6.3](https://tools.ietf.org/html/rfc7231#section-6.6.3)"),
  (503, `SERVICE_UNAVAILABLE            , "Service Unavailable"            , "[RFC7231, Section 6.6.4](https://tools.ietf.org/html/rfc7231#section-6.6.4)"),
  (504, `GATEWAY_TIMEOUT                , "Gateway Timeout"                , "[RFC7231, Section 6.6.5](https://tools.ietf.org/html/rfc7231#section-6.6.5)"),
  (505, `HTTP_VERSION_NOT_SUPPORTED     , "HTTP Version Not Supported"     , "[RFC7231, Section 6.6.6](https://tools.ietf.org/html/rfc7231#section-6.6.6)"),
  (506, `VARIANT_ALSO_NEGOTIATES        , "Variant Also Negotiates"        , "[RFC2295](https://tools.ietf.org/html/rfc2295)"),
  (507, `INSUFFICIENT_STORAGE           , "Insufficient Storage"           , "[RFC4918](https://tools.ietf.org/html/rfc4918)"),
  (508, `LOOP_DETECTED                  , "Loop Detected"                  , "[RFC5842](https://tools.ietf.org/html/rfc5842)"),

  (510, `NOT_EXTENDED                   , "Not Extended"                   , "[RFC2774](https://tools.ietf.org/html/rfc2774)"),
  (511, `NETWORK_AUTHENTICATION_REQUIRED, "Network Authentication Required", "[RFC6585](https://tools.ietf.org/html/rfc6585)")
]
end

instance : Inhabited StatusCode := ⟨CONTINUE⟩

def parse : Parser StatusCode := do
  let ds ← take 3 (tokenFilter Char.isDigit)
  let num := ds.foldl (·*10 + ·.toNat - '0'.toNat) 0

  if h : 100 ≤ num then
    if h2 : num < 1000 then
      return ⟨⟨num, Nat.lt_trans h2 (by decide)⟩, h, h2⟩
    else
      panic! "Http.StatusCode.parse num is ≥ 1000"
  else
    throwUnexpected
