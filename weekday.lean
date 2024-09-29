namespace weekday

inductive Weekday where
  | monday    : Weekday
  | tuesday   : Weekday
  | wednesday : Weekday
  | thursday  : Weekday
  | friday    : Weekday
  | saturday  : Weekday
  | sunday    : Weekday

def number_of_day : Weekday -> Nat :=
  λ
    | .monday    => 1
    | .tuesday   => 2
    | .wednesday => 3
    | .thursday  => 4
    | .friday    => 5
    | .saturday  => 6
    | .sunday    => 7

def is_weekend : Weekday -> Bool :=
  λ
    | .sunday   => true
    | .saturday => true
    | _ => false

instance : ToString Weekday where
  toString : Weekday -> String :=
    λ
      | .monday    => "Monday"
      | .tuesday   => "Tuesday"
      | .wednesday => "Wednesday"
      | .thursday  => "Thursday"
      | .friday    => "Friday"
      | .saturday  => "Saturday"
      | .sunday    => "Sunday"

def next_day : Weekday -> Weekday :=
  λ
    | .monday    => .tuesday
    | .tuesday   => .wednesday
    | .wednesday => .thursday
    | .thursday  => .friday
    | .friday    => .saturday
    | .saturday  => .sunday
    | .sunday    => .monday

def previous_day : Weekday -> Weekday :=
  λ
    | .monday    => .sunday
    | .tuesday   => .monday
    | .wednesday => .tuesday
    | .thursday  => .wednesday
    | .friday    => .thursday
    | .saturday  => .friday
    | .sunday    => .saturday

theorem next_or_previous (day : Weekday) : next_day ( previous_day day ) = day :=by
  cases day
  all_goals rfl

end weekday
