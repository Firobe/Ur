open Tgl4

let pi = 3.14159265359
let trig_norm x = 2. *. pi *. x
let trig_counter i n = trig_norm @@ (float i) /. (float n)
let smooth_progress prog = (cos (pi +. prog *. pi) +. 1.) /. 2. 

let triangle =
  (Gl.lines,
   [| -1.0; -1.0; 0.0;
      1.0; -1.0; 0.0;
      1.0;  1.0; 0.0;
      -1.0;  1.0; 0.0 |],
   [| 1.0; 0.0; 0.0;
      0.0; 1.0; 0.0;
      0.0; 0.0; 1.0;
      1.0; 0.0; 1.0 |],
   [| 0; 1; 2;
      0; 2; 3 |])

let sqare =
  (Gl.line_strip,
   [| 0.; 0.; 0.;
      1.; 0.; 0.;
      1.; 1.; 0.;
      0.; 1.; 0. |],
   [| 1.; 1.; 1.;
      1.; 1.; 1.;
      1.; 1.; 1.;
      1.; 1.; 1. |],
   [| 0; 1; 2; 3; 4 |])

let set_3d a i x y z =
  let start = i * 3 in
  a.(start) <- x; a.(start + 1) <- y; a.(start + 2) <- z

module Counter = struct
  type kind = Raw | Trig | Cos | Sin
  type t = {
    mutable n : int;
    max : int;
    kind : kind;
  }
  let create kind max = {n = 0; max; kind}
  let incr t = t.n <- (t.n + 1) mod t.max
  let get t = match t.kind with
    | Raw -> float t.n
    | Trig -> trig_counter t.n t.max
    | Cos -> cos (trig_counter t.n t.max)
    | Sin -> sin (trig_counter t.n t.max)
end

let circle r g b n =
  let vertices = Array.make ((n + 2) * 3) 0. in
  let colors = Array.make ((n + 2) * 3) 0. in
  for i = 1 to n + 1 do
    let step = trig_counter (i - 1) n in
    set_3d vertices i (cos step) (sin step) 0.;
    set_3d colors i r g b
  done;
  let indices = Array.init (n + 2) (fun x -> x) in
  (Gl.triangle_fan, vertices, colors, indices)

module Pawn = struct
  type t = {p1: Geometry.t; p2: Geometry.t; c: Geometry.t}

  let create () =
    let p1 = Geometry.of_arrays @@ circle 1.0 0. 0. 200 in
    let p2 = Geometry.of_arrays @@ circle 0. 0. 1. 200 in
    let c = Geometry.of_arrays @@ circle 0. 1. 0. 200 in
    {p1; p2; c}

  let pawn_to_float pawn =
    let pawn_to_coord = function
      | {Game.Logic.owner=P1; position=Intro x} -> 3 - x, 0
      | {owner=P2; position=Intro x} -> 3 - x, 2
      | {owner=P1; position=Outro x} -> 6 + (1 - x), 0
      | {owner=P2; position=Outro x} -> 6 + (1 - x), 2
      | {position=Main x; _} -> x, 1
      | {owner=P1; position=Reserve} -> 4, 0
      | {owner=P2; position=Reserve} -> 4, 2
    in
    let nx, ny = pawn_to_coord pawn in
    float nx +. 0.5, float ny +. 0.5

  let draw pid t ?animate ?(choice = false) pawn =
    (* Grille 8 x 3 *)
    let p = if choice then t.c else
      if Game.Logic.(pawn.owner) = P1 then t.p1 else t.p2 in
    let def_scale = if choice then 0.45 else 0.4 in
    let x, y, prog = match animate with
      | None -> let x,y = pawn_to_float pawn in x, y, 0.
      | Some (pawn', prog) ->
        let prog' = smooth_progress prog in
        let x1, y1 = pawn_to_float pawn in
        let x2, y2 = pawn_to_float pawn' in
        x1 +. (x2 -. x1) *. prog',
        y1 +. (y2 -. y1) *. prog',
        prog
    in
    let scafa = 1. +. cos (pi /. 2. +. pi *. prog) /. 3. in
    let trans =
      Matrix.translation x y 0.
      |> Matrix.scale def_scale def_scale 0. 
      |> Matrix.scale scafa scafa 0.
    in Geometry.draw ~trans pid p

  let delete t =
    Geometry.delete t.p1;
    Geometry.delete t.p2
end
