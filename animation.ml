type kind =
  | Title
  | Wait
  | Pawn_moving of (Game.Logic.pawn * Game.Logic.move)
  | Menu_move of (int * int)
  | Cannot_choose of (int * int * int * int)
  | Victory
  | Score_up of Game.playerNo
  | Choice

type t = {id: int; length: float; start: float; kind: kind}

let is_active t =
  let now = Unix.gettimeofday () in
  now -. t.start <= t.length

let new_id = ref 0

let create ?(delay = 0.) ?(speed = 1.) length kind =
  let id = !new_id in
  incr new_id ;
  let start = Unix.gettimeofday () +. delay in
  {id; length= length /. speed; start; kind}

let progress t =
  let now = Unix.gettimeofday () in
  if now < t.start then 0.
  else if now > t.start +. t.length then 1.
  else (now -. t.start) /. t.length
