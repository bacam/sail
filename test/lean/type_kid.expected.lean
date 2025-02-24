import Out.Sail.Sail
import Out.Sail.BitVec

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 10_000
set_option linter.unusedVariables false
set_option match.ignoreUnusedAlts true

open Sail

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource Unit

namespace Functions

/-- Type quantifiers: k_a : Type -/
def foo (x : k_a) : (k_a × k_a) :=
  (x, x)

def initialize_registers (_ : Unit) : Unit :=
  ()

end Functions

open Functions

