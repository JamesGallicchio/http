/-  Copyright (C) 2023 The Http library contributors

    Authors: James Gallicchio
-/

/-- A case-insensitive string.

This should be implemented as a quotient, but since
`String` has very few provable results it is not immediately
feasible.

TODO: modify to work with `ByteArray`s directly. -/
structure CaseInsString where
  val : String
  hval : ∃ s1 : String, val = s1.toLower
deriving DecidableEq, Hashable, Repr

namespace CaseInsString

def ofString (s : String) : CaseInsString := ⟨s.toLower, s, rfl⟩

instance : Inhabited CaseInsString := ⟨ofString ""⟩
variable (s : CaseInsString)

def toLower := s.val
def length := s.val.length
def get := s.val.get

instance : ToString CaseInsString := ⟨toLower⟩
