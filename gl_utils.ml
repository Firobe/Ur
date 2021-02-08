open Tgl4

let ( let* ) x f = match x with Ok v -> f v | Error _ as e -> e
let bigarray_of k a = Bigarray.(Array1.of_array k c_layout a)
let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let set_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f i ->
    a.{0} <- Int32.of_int i ;
    f a

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a ;
    Int32.to_int a.{0}

let create_buffer b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id ;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw ;
  id

let delete_buffer bid = set_int (Gl.delete_buffers 1) bid

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a ; Gl.string_of_bigarray a

let str = Printf.sprintf

let text_rectangle x y w h =
  ( Gl.triangles
  , [|x; y; 0.; x +. w; y; 0.; x +. w; y +. h; 0.; x; y +. h; 0.|]
  , [|0.; 1.; 1.; 1.; 1.; 0.; 0.; 0.|]
  , [|0; 1; 3; 2; 1; 3|] )

let color_to_floats (r, g, b) =
  (float r /. 255., float g /. 255., float b /. 255.)
