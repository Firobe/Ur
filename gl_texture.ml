open Tsdl
open Gl_utils
open Tgl4

type t = {tid: int; surface: Sdl.surface; width: int; height: int}

let width t = t.width
let height t = t.height

let create_from_surface raw_surface =
  let kind = Bigarray.Int8_unsigned in
  let dest_format = Sdl.Pixel.format_argb8888 in
  let* surface = Sdl.convert_surface_format raw_surface dest_format in
  Sdl.free_surface raw_surface ;
  let* _ = Sdl.lock_surface surface in
  let width, height = Sdl.get_surface_size surface in
  let pixels = Sdl.get_surface_pixels surface kind in
  let tid = get_int (Gl.gen_textures 1) in
  Gl.bind_texture Gl.texture_2d tid ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_min_filter Gl.linear ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_s Gl.repeat ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_wrap_t Gl.repeat ;
  Gl.tex_parameteri Gl.texture_2d Gl.texture_mag_filter Gl.linear ;
  Gl.tex_image2d Gl.texture_2d 0 Gl.rgba width height 0 Gl.bgra Gl.unsigned_byte
    (`Data pixels) ;
  Gl.bind_texture Gl.texture_2d 0 ;
  Ok {tid; surface; width; height}

let create_from_bmp path =
  let* surface =
    match Tsdl_image.Image.load path with
    | Some x -> Result.ok x
    | None -> Result.error (`Msg ("Could not load texture " ^ path))
  in
  create_from_surface surface

let delete t =
  set_int (Gl.delete_textures 1) t.tid ;
  Sdl.free_surface t.surface

let bind t = Gl.bind_texture Gl.texture_2d t.tid
