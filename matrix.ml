open Gl_utils

type t = float array

(* Transformations *)

(* Transpose before sending to OpenGL*)
let ortho l r b t n f =
  [| 2. /. (r -. l); 0.; 0.; -.(r +. l) /. (r -. l); 0.; 2. /. (t -. b); 0.
   ; -.(t +. b) /. (t -. b); 0.; 0.; -2. /. (f -. n); -.(f +. n) /. (f -. n); 0.
   ; 0.; 0.; 1. |]

let identity = [|1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.|]

let scaling sx sy sz =
  [|sx; 0.; 0.; 0.; 0.; sy; 0.; 0.; 0.; 0.; sz; 0.; 0.; 0.; 0.; 1.|]

let translation tx ty tz =
  [|1.; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.; 0.; tx; ty; tz; 1.|]

let rotation_x a =
  let ca = cos a in
  let sa = sin a in
  let ma = -.sa in
  [|1.; 0.; 0.; 0.; 0.; ca; ma; 0.; 0.; sa; ca; 0.; 0.; 0.; 0.; 1.|]

let rotation_y a =
  let ca = cos a in
  let sa = sin a in
  let ma = -.sa in
  [|ca; 0.; sa; 0.; 0.; 1.; 0.; 0.; ma; 0.; ca; 0.; 0.; 0.; 0.; 1.|]

let rotation_z a =
  let ca = cos a in
  let sa = sin a in
  let ma = -.sa in
  [|ca; ma; 0.; 0.; sa; ca; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1.|]

(* Operations *)
let range = [0; 1; 2; 3]

let mult a b =
  let f n =
    let i = n / 4 in
    let j = n mod 4 in
    List.fold_left
      (fun sum k -> (a.((i * 4) + k) *. b.((k * 4) + j)) +. sum)
      0. range in
  Array.init (4 * 4) f

let scale sx sy sz t = mult (scaling sx sy sz) t
let translate tx ty tz t = mult (translation tx ty tz) t
let rotate_x a t = mult (rotation_x a) t
let rotate_y a t = mult (rotation_y a) t
let rotate_z a t = mult (rotation_z a) t

let pp fmt t =
  for i = 0 to 3 do
    for j = 0 to 3 do
      Format.fprintf fmt "%-6.3g " t.((i * 4) + j)
    done ;
    Format.fprintf fmt "@."
  done

let raw t = bigarray_of Bigarray.float32 t
