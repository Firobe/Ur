type kind =
  | Wait
  | Pawn_moving of (Game.Logic.pawn * Game.Logic.move)

type t = {
  id : int;
  length : float;
  start : float;
  kind : kind;
}

let is_active t = 
  let now = Unix.gettimeofday () in
  now -. t.start <= t.length

let new_id = ref 0

let create length kind =
  let id = !new_id in
  incr new_id;
  let start = Unix.gettimeofday () in
  {id; length; start; kind}

let progress t =
  let now = Unix.gettimeofday () in
  (now -. t.start) /. t.length
