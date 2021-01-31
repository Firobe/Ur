open Bigarray
open Tgl4
open Gl_utils

type int_bigarray = (int, int8_unsigned_elt, c_layout) Array1.t
type float_bigarray = (float, float32_elt, c_layout) Array1.t

type raw =
  { vertices: float_bigarray
  ; frags: (float_bigarray * [`Colored | `Textured])
  ; indices: int_bigarray
  ; mode: int }

type t = {gid: int; bids: int list; length: int; mode: int}

let create raw =
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer raw.indices in
  let vid = create_buffer raw.vertices in
  let cid = create_buffer (fst raw.frags) in
  let mode = raw.mode in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id ;
    Gl.enable_vertex_attrib_array loc ;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0) in
  Gl.bind_vertex_array gid ;
  Gl.bind_buffer Gl.element_array_buffer iid ;
  bind_attrib vid 0 3 Gl.float ;
  begin match (snd raw.frags) with
  | `Colored -> bind_attrib cid 1 3 Gl.float 
  | `Textured -> bind_attrib cid 1 2 Gl.float
  end ;
  Gl.bind_vertex_array 0 ;
  Gl.bind_buffer Gl.array_buffer 0 ;
  Gl.bind_buffer Gl.element_array_buffer 0 ;
  let result =
    {gid; bids= [iid; vid; cid]; length= Bigarray.Array1.dim raw.indices; mode}
  in
  result

let of_arrays ?(frag_kind=`Colored) (mode, v, c, i) =
  let raw =
    { vertices= bigarray_of Bigarray.float32 v
    ; frags= (bigarray_of Bigarray.float32 c, frag_kind)
    ; indices= bigarray_of Bigarray.int8_unsigned i
    ; mode } in
  create raw

let delete t =
  set_int (Gl.delete_vertex_arrays 1) t.gid ;
  List.iter delete_buffer t.bids

let draw ?(trans = Matrix.identity) pid t =
  Gl.use_program pid ;
  let matid = Gl.get_uniform_location pid "model" in
  Gl.uniform_matrix4fv matid 1 false (Matrix.raw trans) ;
  Gl.bind_vertex_array t.gid ;
  Gl.draw_elements t.mode t.length Gl.unsigned_byte (`Offset 0) ;
  Gl.bind_vertex_array 0
