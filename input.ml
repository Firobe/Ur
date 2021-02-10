type position =
  | Reserve
  | Intro of int (* [0-3] *)
  | Main of int (* [0-7] *)
  | Outro of int

(* [0-1] *)

type t =
  | Pawn of position
  | Throw_dices
  | Quit
  | Error of string
  | Validate
  | Next_menu
  | Previous_menu
  | Next_option
  | Previous_option

let coord_of_raw ss h off_x off_y rx ry =
  let ry = h - ry in
  let x = (rx / ss) - off_x in
  let y = (ry / ss) - off_y in
  (x, y)

let coord_in_board x y = x >= 0 && y >= 0 && x <= 7 && y <= 2
let coord_in_cup x y = x < 0 && y < 1

let coord_to_pos x y =
  if y = 1 then Main x else if x <= 3 then Intro (3 - x) else Outro (7 - x)
