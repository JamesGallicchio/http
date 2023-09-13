import Lake
open Lake DSL

package Http

@[default_target]
lean_lib Http

require Qq from git "https://github.com/gebner/quote4" @ "master"
require std from git "https://github.com/leanprover/std4" @ "main"
require Parser from git "https://github.com/fgdorais/lean4-parser" @ "main"
