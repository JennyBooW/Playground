-- Length function.

def length : List α -> Nat
  | [] => 0
  | _ :: ys => Nat.succ (length ys)

#eval length [1, 2, 3]

-- Drop function.

def drop : Nat -> List α -> List α
  | Nat.zero, xs => xs
  | _, [] => []
  | Nat.succ n, _ :: ys => drop n ys

#eval drop 1 [1, 2, 3, 4, 5]


def from_option (default : α) : Option α -> α
  | none   => default
  | some x => x

#eval (some "Holly").getD ""

-- Unzip function

def unzip : List (α × β) -> List α × List β
  | [] => ([], [])
  | (x, y) :: xyz =>
    let unzipped : List α × List β := unzip xyz
    (x :: unzipped.fst, y :: unzipped.snd)

-- One constructor so we dont need to explicitly give it a name.

def unzip_better : List (α × β ) -> List α × List β
  | [] => ([], [])
  | (x, y) :: xyz =>
    let (xs, ys) : List α × List β := unzip xyz
    (x :: xs, y :: ys)

#check (unzip)
#check (unzip_better)

-- rec keyword to make let recursive

def reverse (xs : List α) : List α :=
  let rec helper : List α -> List α -> List α
    | [], soFar => soFar
    | y :: ys, soFar => helper ys (y :: soFar)
  helper xs []

#eval reverse [1, 2, 3, 4, 5]

-- Multiple patter matching

def drop_better (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | Nat.zero, ys => ys
  | _, [] => []
  | Nat.succ k, _ :: ys => drop k ys

#eval drop_better 1 [1,2,3,4,5]

-- Natural numbers patterns

def even : Nat -> Bool
  | 0 => true
  | n + 1 => not (even n)

#eval even 1

def halve : Nat -> Nat
  | 0 => 0
  | 1 => 0
  | n + 2 => halve n + 1

#eval halve 4

-- Anonmous functions

#check fun x => x + 1

#check λ
  | 0 => none
  | n + 1 => some n

def double : Nat -> Nat := λ
  | 0 => 0
  | k + 1 => double k + 2

#eval double 10

-- Namespaces

namespace my_namespace
def triple : Nat -> Nat
  | 0 => 0
  | k + 1 => triple k + 3

def quadruple (n : Nat) : Nat := 2 * n + n * 2

end my_namespace

#eval my_namespace.triple    10
#eval my_namespace.quadruple 10

def times_twelve (n : Nat) : Nat :=
  open my_namespace in
  quadruple (triple n)

#eval times_twelve 10


-- String inrepolation

#eval s!"Five times twelve is: {times_twelve 5}"
