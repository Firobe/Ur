(* ==== GLOBAL PARAMETERS ==== *)

let max_pawns = 7
let ss = 40 (* Square size *)
let auto_mode = false
let ia_wait = if auto_mode then 0.15 else 1.

(* ==== TYPES ==== *)

type playerNo = P1 | P2

type position =
  | Reserve
  | Intro of int (* [0-3] *)
  | Main of int  (* [0-7] *)
  | Outro of int (* [0-1] *)

type pawn = {
  owner : playerNo;
  position : position;
}

type move =
  | Add
  | Move of position
  | Take of pawn
  | Finish

type player = {
  reserve : int;
  points : int;
  choose : (pawn * move) list -> (pawn * move) option
}

type state = {
  p1 : player;
  p2 : player ;
  pawns : pawn list;
}

