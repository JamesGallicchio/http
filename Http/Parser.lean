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

import Parser

namespace Http

nonrec abbrev Parser.Error := Parser.Error.Simple Substring Char
nonrec abbrev Parser := Parser Parser.Error Substring Char

def Parser.capture (p : Parser α) : Parser Substring := do
  let p1 ← Parser.getPosition
  let _ ← p
  let p2 ← Parser.getPosition
  return { (← StateT.get).stream with
      startPos := p1
      stopPos := p2
    }

def Parser.ws : Parser Unit := do
  Parser.dropMany <| Parser.tokenFilter <| Char.isWhitespace
