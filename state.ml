type t =
  | Playing of Game.t
  | End

let has_quit inputs =
  List.exists ((=) Input.Quit) inputs

let reducer state inputs =
  Thread.delay 0.00001; match state with
  | Playing {gameplay = Victory _; _} -> End
  | Playing game ->
    if has_quit inputs then End
    else Playing (Game.next game)
  | End -> End

