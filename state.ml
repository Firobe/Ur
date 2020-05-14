type t =
  | Playing of Game.t
  | End

let reducer state =
  Thread.delay 0.1; match state with
  | Playing {gameplay = Victory _; _} -> End
  | Playing game -> Playing (Game.next game)
  | End -> End

