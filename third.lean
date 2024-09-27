-- Cartesian point
structure Point where
  x : Float
  y : Float
deriving Repr

-- Origin point
def origin : Point := { x := 0.0, y := 0.0}

-- Two points
def x : Point := { x := 5.5, y :=  6.4 }
def y : Point := { x := 10,  y := -9.9 }

-- Function for work with 2D points.
def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p1.y}

-- Distance between two points.
def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt ( ((p2.x - p1.x) ^ 2) + ((p2.y - p1.y) ^ 2) )

#eval addPoints x y
#eval distance  x y

#check { x := 0.0, y := 0.0  : Point }


def zeroX (p : Point) : Point :=
  { p with x := 0 }

#eval zeroX x

-- Override constructor name
structure Point_cool where
  point ::
  x : Float
  y : Float
deriving Repr

-- Add a function to the Point namespace
-- Function takes a Function and applies to both points.
def Point.modify_both (f : Float -> Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

#eval x.modify_both Float.abs

-- EXERCIESES

-- Define a structure named RectangularPrism that contains
-- the height, width, and depth of a rectangular prism, each as a Float.
structure RectangularPrism where
  prism ::
  height : Float
  width  : Float
  depth  : Float
deriving Repr

def prismABC : RectangularPrism :=
  { height := 10, width := 10, depth := 10 }

-- Define a function named volume : RectangularPrism → Float
-- that computes the volume of a rectangular prism.

def volume (r : RectangularPrism) : Float :=
  0.5 * r.height * r.depth * r.width


#eval volume prismABC

-- Define a structure named Segment that represents
-- a line segment by its endpoints,

structure Segment where
  segment ::
  right : Float
  left  : Float
deriving Repr

def seg : Segment :=
  { right := 10, left := 5 }

-- and define a function length : Segment → Float that computes
-- the length of a line segment. Segment should have at most two fields.

def Length (s : Segment) : Float :=
  Float.abs (s.right - s.left)

#eval Length seg

-- Which names are introduced by the declaration of RectangularPrism?
-- Which names are introduced by the following declarations of Hamster and Book? What are their types?

structure Hamster where
  name   : String
  fluffy : Bool

structure Book where
  makeBook ::
  title  : String
  author : String
  price  : Float
