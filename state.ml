type t =
  | Playing of Game.t
  | End

let reducer = function
  | Playing {gameplay = Victory _; _} -> End
  | Playing game -> Playing (Game.next game)
  | End -> End

