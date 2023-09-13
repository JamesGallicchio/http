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

/-- A case-insensitive string.

This should be implemented as a quotation, but since
`String` has very few provable results it is not immediately
feasible.

TODO: modify to work with `ByteArray`s directly. -/
structure CaseInsString where
  val : String
  hval : ∃ s1 : String, val = s1.toLower
deriving DecidableEq, Hashable, Repr

namespace CaseInsString

def ofString (s : String) : CaseInsString := ⟨s.toLower, s, rfl⟩

variable (s : CaseInsString)

def toLower := s.val
def length := s.val.length
def get := s.val.get

instance : ToString CaseInsString := ⟨toLower⟩
