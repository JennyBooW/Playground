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
  | _ :: ys => Nat.succ (length alpha ys)

#eval length Nat primes_under_ten

-- Type inference
def replace_x {alpha : Type} (point : PPoint alpha) (new_x : alpha) :=
  { point with x := new_x }

def str_point : PPoint String :=
  { x := "Hello", y := "Lean!"}

#eval replace_x str_point "Nice"

def words : List String := ["Hello", " ", "Lean", "!"]

def length_inference {alpha : Type} (xs : List alpha) : Nat :=
  match xs with
  | []      => Nat.zero
  | _ :: ys => Nat.succ (length_inference ys)


#eval words.headD ""

#eval [].head? ( α := Int )

-- Product structure simliar to Pair and Triple in Kotlin

def prd : String × Int := ("Hello", 5)

-- Sum something like a Unit in C ?

def pet_name : Type := String ⊕ String

def animals : List pet_name :=
  [ Sum.inl "Lammy", Sum.inr "Catty", Sum.inl "Spot", Sum.inr "Dorothy" ]

#eval animals.head?

-- Count dogs

def count_dogs (pets : List pet_name) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: more => count_dogs more + 1
  | Sum.inr _ :: more => count_dogs more

#eval count_dogs animals

-- Unit  is like void ?
-- Empty is null !

-- EXERCISES

-- Write a function to find the last entry in a list. It should return an Option.

-- Test lists
def numbers : List Nat      := [1, 2, 3, 4, 5]
def letters : List String   := ["A", "B", "C", "D"]
def empty   : List Int      := []

def find_tail {α : Type} (xs : List α) : Option α :=
  match xs with
  | []      => none
  | [x]     => some x
  | _ :: ys => find_tail ys

#eval find_tail numbers
#eval find_tail letters
#eval find_tail empty

-- Write a function that finds the first entry in a list that satisfies a given
-- predicate. Start the definition with def List.findFirst?
-- {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=

def findFirst {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | []    => none
  | y::ys => if predicate y
    then y
    else findFirst ys predicate

-- Test predicate function
def isEven (n : Nat) : Bool :=
  match n with
  | Nat.zero   => true
  | Nat.succ k => not ( isEven k )

#eval findFirst numbers isEven

-- Write a function Prod.swap that swaps the two fields in a pair.
-- Start the definition with def Prod.swap {α β : Type} (pair : α × β) : β × α :=

def swap {α β : Type} (pair : α × β) : β × α :=
  (pair.snd, pair.fst)

#eval swap prd

-- Rewrite the PetName example to use a custom datatype and compare it to the version
-- that uses Sum.

def custom_dog_cat {α β : Type} (x : α) (y : β) : α × β :=
  (x, y)


def my_animals : List (String × String) :=
  [ custom_dog_cat "Lammy" "Catty",  custom_dog_cat "Spot" "Dorothy"]

#eval my_animals.head?


-- Write a function zip that combines two lists into a list of pairs. The resulting list should be as long as the shortest input list. Start the definition with

def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | [], _ => []
  | _, [] => []
  | x :: xs', y :: ys' => (x, y) :: zip xs' ys'

def abc : List String := ["A", "B", "C"]
def xyz : List String := ["X", "Y", "Z"]

#eval zip abc xyz

-- Write a polymorphic function "take" that returns the first n
--  entries in a list, where n is a Nat. If the list contains fewer than n entries,
-- then the resulting list should be the input list. #eval take 3 ["bolete", "oyster"]
-- should yield ["bolete", "oyster"], and #eval take 1 ["bolete", "oyster"]
-- should yield ["bolete"].

def take {α : Type } (xs : List α) (n : Nat) : List α :=
  match xs, n with
  | [], _  => []
  |  _, 0  => []
  | x :: xs', k' => x :: take xs' (k' - 1)

#eval take letters 1

-- Using the analogy between types and arithmetic, write a function that
-- distributes products over sums. In other words, it should have
-- type α × (β ⊕ γ) → (α × β) ⊕ (α × γ).

def distributes {α β γ : Type} : Prod α (Sum β γ) → Sum (Prod α β) (Prod α γ)
  | (a, Sum.inl b) => Sum.inl (a, b)
  | (a, Sum.inr c) => Sum.inr (a, c)

#check distributes

-- Using the analogy between types and arithmetic, write a
-- function that turns multiplication by two into a sum. In other words,
-- it should have type Bool × α → α ⊕ α.

def multiply_by_two {α : Type} : Bool × α → Sum α α
| (true, a)  => Sum.inl a
| (false, a) => Sum.inr a
#check multiply_by_two
