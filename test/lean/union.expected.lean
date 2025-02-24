import Out.Sail.Sail
import Out.Sail.BitVec

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 10_000
set_option linter.unusedVariables false
set_option match.ignoreUnusedAlts true

open Sail


structure rectangle where
  width : Int
  height : Int
  deriving BEq


structure circle where
  radius : Int
  deriving BEq


inductive shape where
  | Rectangle (_ : rectangle)
  | Circle (_ : circle)
  deriving BEq

open shape

/-- Type quantifiers: k_a : Type -/

inductive my_option (k_a : Type) where
  | MySome (_ : k_a)
  | MyNone (_ : Unit)
  deriving BEq

open my_option

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource Unit

namespace Functions

def undefined_rectangle (_ : Unit) : SailM rectangle := do
  (pure { width := (← (undefined_int ()))
          height := (← (undefined_int ())) })

def undefined_circle (_ : Unit) : SailM circle := do
  (pure { radius := (← (undefined_int ())) })

/-- Type quantifiers: k_a : Type -/
def is_none (opt : (my_option k_a)) : Bool :=
  match opt with
  | .MySome _ => false
  | .MyNone () => true

/-- Type quantifiers: k_a : Type -/
def use_is_none (opt : (my_option k_a)) : Bool :=
  (is_none opt)

def initialize_registers (_ : Unit) : Unit :=
  ()

end Functions

open Functions

