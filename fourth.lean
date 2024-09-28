-- Datatypes and Patterns


structure Point3D where
  point3d ::

  height  : Float
  width   : Float
  depth   : Float
deriving Repr

def depth (p : Point3D) : Float :=
  match p with
  | { depth := d, height, width } => d

def x : Point3D :=
  { height := 2, width := 2, depth := 10 }

#eval depth x

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero   => true
  | Nat.succ k => not ( even k )

#eval even 10


def plus (x : Nat) (y : Nat) : Nat :=
  match y with
  | Nat.zero    => x
  | Nat.succ y' => Nat.succ (plus x y')

#eval plus 1 1
