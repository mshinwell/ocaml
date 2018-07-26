

type changes = Neither | Left | Right | Both

let join_changes (changes1 : changes) (changes2 : changes) =
  match changes1, changes2 with
  | Neither, Neither -> Neither
  | Neither, Left -> Left
  | Neither, Right -> Right
  | Neither, Both -> Both
  | Left, Neither -> Left
  | Left, Left -> Left
  | Left, Right -> Both
  | Left, Both -> Both
  | Right, Neither -> Right
  | Right, Left -> Both
  | Right, Right -> Right
  | Right, Both -> Both
  | Both, Neither -> Both
  | Both, Left -> Both
  | Both, Right -> Both
  | Both, Both -> Both
end
