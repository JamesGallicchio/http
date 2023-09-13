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

import Http.Parser

open Parser

namespace Http

inductive Version
| HTTP_0_9
| HTTP_1_0
| HTTP_1_1
| HTTP_2_0
| HTTP_3_0

namespace Version

instance : Inhabited Version := ⟨HTTP_1_1⟩

def toString : Version → String
| HTTP_0_9 => "HTTP/0.9"
| HTTP_1_0 => "HTTP/1.0"
| HTTP_1_1 => "HTTP/1.1"
| HTTP_2_0 => "HTTP/2.0"
| HTTP_3_0 => "HTTP/3.0"

instance : ToString Version := ⟨toString⟩

def parse : Parser Version :=
  token 'H' *> token 'T' *> token 'T' *> token 'P' *> token '/' *>
  tokenFilter Char.isDigit >>= fun d1 =>
    token '.' *>
    tokenFilter Char.isDigit >>= fun d2 =>
      match d1.toNat - '0'.toNat, d2.toNat - '0'.toNat with
      | 0, 9 => pure .HTTP_0_9
      | 1, 0 => pure .HTTP_1_0
      | 1, 1 => pure .HTTP_1_1
      | 2, 0 => pure .HTTP_2_0
      | 3, 0 => pure .HTTP_3_0
      | _, _ => throwUnexpected
