/-  Copyright (C) 2023 The Http library contributors

    Authors: James Gallicchio
-/

import Parser

namespace Http

nonrec abbrev Parser.Error := Parser.Error.Simple Substring Char
nonrec abbrev Parser := Parser Parser.Error Substring Char

-- def Parser.capture (p : Parser α) : Parser Substring := do
--   let p1 ← Parser.getPosition
--   let _ ← p
--   let p2 ← Parser.getPosition
--   return { (← StateT.get).stream with
--       startPos := p1
--       stopPos := p2
--     }

def Parser.ws : Parser Unit := do
  Parser.dropMany <| Parser.tokenFilter <| Char.isWhitespace
