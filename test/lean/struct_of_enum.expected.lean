import Out.Sail.Sail
import Out.Sail.BitVec

set_option maxHeartbeats 1_000_000_000
set_option maxRecDepth 10_000
set_option linter.unusedVariables false

open Sail

abbrev bits k_n := (BitVec k_n)

/-- Type quantifiers: k_a : Type -/

inductive option (k_a : Type) where
  | Some (_ : k_a)
  | None (_ : Unit)
  deriving BEq

open option

inductive e_test where | VAL
  deriving Inhabited, BEq

open e_test


structure s_test where
  f : e_test
  deriving BEq

abbrev SailM := PreSailM PEmpty.elim trivialChoiceSource Unit

namespace Functions

/-- Type quantifiers: k_ex705# : Bool, k_ex704# : Bool -/
def neq_bool (x : Bool) (y : Bool) : Bool :=
  (Bool.not (BEq.beq x y))

/-- Type quantifiers: x : Int -/
def __id (x : Int) : Int :=
  x

/-- Type quantifiers: len : Nat, k_v : Nat, len ≥ 0 ∧ k_v ≥ 0 -/
def sail_mask (len : Nat) (v : (BitVec k_v)) : (BitVec len) :=
  if (LE.le len (Sail.BitVec.length v))
  then (Sail.BitVec.truncate v len)
  else (Sail.BitVec.zeroExtend v len)

/-- Type quantifiers: n : Nat, n ≥ 0 -/
def sail_ones (n : Nat) : (BitVec n) :=
  (Complement.complement (BitVec.zero n))

/-- Type quantifiers: l : Int, i : Int, n : Nat, n ≥ 0 -/
def slice_mask {n : _} (i : Int) (l : Int) : (BitVec n) :=
  if (GE.ge l n)
  then ((sail_ones n) <<< i)
  else let one : (BitVec n) := (sail_mask n (0b1 : (BitVec 1)))
       (((one <<< l) - one) <<< i)

/-- Type quantifiers: n : Int, m : Int -/
def _shl_int_general (m : Int) (n : Int) : Int :=
  if (GE.ge n 0)
  then (Int.shiftl m n)
  else (Int.shiftr m (Neg.neg n))

/-- Type quantifiers: n : Int, m : Int -/
def _shr_int_general (m : Int) (n : Int) : Int :=
  if (GE.ge n 0)
  then (Int.shiftr m n)
  else (Int.shiftl m (Neg.neg n))

/-- Type quantifiers: m : Int, n : Int -/
def fdiv_int (n : Int) (m : Int) : Int :=
  if (Bool.and (LT.lt n 0) (GT.gt m 0))
  then ((Int.tdiv (n + 1) m) - 1)
  else if (Bool.and (GT.gt n 0) (LT.lt m 0))
       then ((Int.tdiv (n - 1) m) - 1)
       else (Int.tdiv n m)

/-- Type quantifiers: m : Int, n : Int -/
def fmod_int (n : Int) (m : Int) : Int :=
  (n - (m * (fdiv_int n m)))

/-- Type quantifiers: k_a : Type -/
def is_none (opt : (Option k_a)) : Bool :=
  match opt with
  | .some _ => false
  | none => true

/-- Type quantifiers: k_a : Type -/
def is_some (opt : (Option k_a)) : Bool :=
  match opt with
  | .some _ => true
  | none => false

/-- Type quantifiers: k_n : Int -/
def concat_str_bits (str : String) (x : (BitVec k_n)) : String :=
  (HAppend.hAppend str (BitVec.toFormatted x))

/-- Type quantifiers: x : Int -/
def concat_str_dec (str : String) (x : Int) : String :=
  (HAppend.hAppend str (Int.repr x))

def undefined_e_test (_ : Unit) : SailM e_test := do
  (internal_pick [VAL])

/-- Type quantifiers: arg_ : Nat, 0 ≤ arg_ ∧ arg_ ≤ 0 -/
def e_test_of_num (arg_ : Nat) : e_test :=
  match arg_ with
  | _ => VAL

def num_of_e_test (arg_ : e_test) : Int :=
  match arg_ with
  | VAL => 0

def undefined_s_test (_ : Unit) : SailM s_test := do
  (pure { f := (← (undefined_e_test ())) })

def initialize_registers (_ : Unit) : Unit :=
  ()

end Functions

open Functions

