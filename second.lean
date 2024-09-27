-- Natural numbers N -> {0, ... }
-- Any n belongs to N, n = ( 0 + 1 ) * n
-- Weird definition ?

#eval (1 - 2 : Nat)
-- Equivalent to uinteger_t in C
-- And BigInteger in Java.

-- Integers
#eval (1 - 2 : Int)


def hello_world : String := "Hello, World"

#eval hello_world

def add (x : Nat) (y : Nat) : Nat := x + y
def succ (x : Nat) : Nat := x + 1

#eval add 10 10

#eval succ 11

def maximum (x : Nat) (y : Nat) : Nat :=
  if x > y then x else y

#eval maximum 1000 1001
#check maximum

abbrev Natural : Type := Nat

#eval (10 - 90 : Natural)
