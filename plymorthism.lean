-- Polymorthism in Lean :)

structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

def natutal_origin_point : PPoint Nat :=
  { x := Nat.zero, y := Nat.zero }

#eval natutal_origin_point


def replaceX (α : Type) (point : PPoint α) (newX : α) :=
  { point with x := newX }

def x_point : PPoint Nat :=
  { x := 10, y := 10 }

#eval replaceX Nat x_point 12
#check (replaceX)

inductive Sign where
  | positive
  | negative

-- COOL >:)

def positive_or_negative (s : Sign) : match s with
  | Sign.positive => Nat
  | Sign.negative => Int :=
    match s with
    | Sign.positive => (3  : Nat)
    | Sign.negative => (-3 : Int)

#eval positive_or_negative Sign.positive
#check (positive_or_negative)

-- MY LIST !

inductive my_list (alpha : Type) where
  | nil : my_list alpha
  | cons : alpha -> my_list alpha -> my_list alpha

def primes_under_ten : List Nat := [2, 3, 5, 7]

#eval primes_under_ten

-- Length of a list
def length (alpha : Type) (xs : List alpha) : Nat :=
  match xs with
  | []      => Nat.zero
  | y :: ys => Nat.succ (length alpha ys)

#eval length Nat primes_under_ten

-- Type inference
def replace_x {alpha : Type} (point : PPoint alpha) (new_x : alpha) :=
  { point with x := new_x }

#eval replace_x x_point 12
