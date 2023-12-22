/-  Copyright (C) 2023 The Http library contributors

    Authors: James Gallicchio
-/

import Lean
import Std

def Lean.mkDocComment (s : String) :=
  mkNode ``Parser.Command.docComment #[mkAtom "/--", mkAtom (s ++ "-/")]

@[inline] def Http.CRLF : String := "\r\n"
