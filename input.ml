type position =
  | Reserve
  | Intro of int (* [0-3] *)
  | Main of int (* [0-7] *)
  | Outro of int

(* [0-1] *)

type t = Pawn of position | Quit | Error of string

let coord_to_pos x y =
  if y = 1 then Main x else if x <= 3 then Intro (3 - x) else Outro (7 - x)
