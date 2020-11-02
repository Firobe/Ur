open Bigarray
open Tgl4
open Glutils

type int_bigarray = (int, int8_unsigned_elt, c_layout) Array1.t
type float_bigarray = (float, float32_elt, c_layout) Array1.t

type raw = {
  vertices : float_bigarray;
  colors : float_bigarray;
  indices : int_bigarray;
}

type t = {
  gid : int;
  bids : int list;
}

let create t =
  let gid = get_int (Gl.gen_vertex_arrays 1) in
  let iid = create_buffer t.indices in
  let vid = create_buffer t.vertices in
  let cid = create_buffer t.colors in
  let bind_attrib id loc dim typ =
    Gl.bind_buffer Gl.array_buffer id;
    Gl.enable_vertex_attrib_array loc;
    Gl.vertex_attrib_pointer loc dim typ false 0 (`Offset 0);
  in
  Gl.bind_vertex_array gid;
  Gl.bind_buffer Gl.element_array_buffer iid;
  bind_attrib vid 0 3 Gl.float;
  bind_attrib cid 1 3 Gl.float;
  Gl.bind_vertex_array 0;
  Gl.bind_buffer Gl.array_buffer 0;
  Gl.bind_buffer Gl.element_array_buffer 0;
  let result = {
    gid;
    bids = [iid; vid; cid]
  } in Ok result

let delete t =
  set_int (Gl.delete_vertex_arrays 1) t.gid;
  List.iter delete_buffer t.bids;
  Ok ()
