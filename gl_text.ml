open Tsdl
open Tsdl_ttf
open Gl_utils
open Tgl4

module Font_index = struct
  type t = string * int

  let compare = Stdlib.compare
end

module Texture_index = struct
  type t = string * int * (int * int * int * int) * string

  let compare = compare
end

module Font_cache = Map.Make (Font_index)
module Texture_cache = Map.Make (Texture_index)

module Text_object = struct
  type t =
    { geometry: Gl_geometry.t
    ; shader: Gl_shader.t
    ; texture: Gl_texture.t
    ; quad_width: float
    ; quad_height: float }

  let v_filename = "shaders/textured.vert"
  let f_filename = "shaders/textured.frag"

  let create data_path proj texture =
    let frag_kind = `Textured in
    (* TODO correctly zone the stuff *)
    let quad_width = float (Gl_texture.width texture) /. 100. in
    let quad_height = float (Gl_texture.height texture) /. 100. in
    let zone = text_rectangle 0. 0. quad_width quad_height in
    let* geometry = Gl_geometry.of_arrays ~frag_kind zone in
    let* shader =
      Gl_shader.create data_path ~v_filename ~f_filename
        ["vertex"; "texture_coords"]
    in
    Gl_shader.send_matrix shader "view" proj ;
    Ok {geometry; shader; texture; quad_width; quad_height}

  let draw t scale x y =
    Gl.bind_texture Gl.texture_2d t.texture.tid ;
    let dx = x -. (scale *. t.quad_width /. 2.) in
    let dy = y -. (scale *. t.quad_height /. 2.) in
    let trans = Matrix.translation dx dy 0. |> Matrix.scale scale scale 1. in
    Gl_geometry.draw ~trans t.shader.pid t.geometry ;
    Gl.bind_texture Gl.texture_2d 0

  let delete t =
    Gl_geometry.delete t.geometry ;
    Gl_shader.delete t.shader ;
    Gl_texture.delete t.texture
end

type t =
  { data_path: string
  ; font_cache: Ttf.font Font_cache.t
  ; texture_cache: Text_object.t Texture_cache.t
  ; default_index: Font_index.t option
  ; proj: Matrix.t }

let init proj data_path =
  if Sdl.Init.test (Sdl.was_init None) Sdl.Init.video then
    let* _ = Ttf.init () in
    let font_cache = Font_cache.empty in
    let texture_cache = Texture_cache.empty in
    Ok {font_cache; texture_cache; default_index= None; proj; data_path}
  else Error (`Msg "Tried to initialize SDL_ttf without initializing SDL")

let add_font cache font_name font_size =
  let* font = Ttf.open_font font_name font_size in
  Ok (Font_cache.add (font_name, font_size) font cache)

let set_default t font_name font_size =
  let default_index = Some (font_name, font_size) in
  {t with default_index}

let get_font_spec t font_name font_size =
  let* font_name =
    match font_name with
    | Some x -> x
    | None ->
        if Option.is_some t.default_index then
          Ok (fst (Option.get t.default_index))
        else Error (`Msg "Must provide font name either as parameter or default")
  in
  let* font_size =
    match font_size with
    | Some x -> x
    | None ->
        if Option.is_some t.default_index then
          Ok (snd (Option.get t.default_index))
        else Error (`Msg "Must provide font size either as parameter or default")
  in
  Ok (font_name, font_size)

let get_font t font_name font_size =
  let* font, cache =
    match Font_cache.find_opt (font_name, font_size) t.font_cache with
    | Some f -> Ok (f, t.font_cache)
    | None ->
        let* font = Ttf.open_font font_name font_size in
        Ok (font, Font_cache.add (font_name, font_size) font t.font_cache)
  in
  Ok (font, cache)

let gen_texture t font_name font_size color text =
  (* TODO manage bold/italic/etc. using Ttf.set_font_style *)
  let* font, font_cache = get_font t font_name font_size in
  let* surface = Ttf.render_utf8_blended font text color in
  let* texture = Gl_texture.create_from_surface surface in
  Ok (texture, {t with font_cache})

let get_obj t font_name font_size color text =
  let key = (font_name, font_size, color, text) in
  match Texture_cache.find_opt key t.texture_cache with
  | Some obj -> Ok (obj, t)
  | None ->
      let r, g, b, a = color in
      let sdl_color = Sdl.Color.create ~r ~g ~b ~a in
      let* texture, t = gen_texture t font_name font_size sdl_color text in
      let* obj = Text_object.create t.data_path t.proj texture in
      let texture_cache = Texture_cache.add key obj t.texture_cache in
      Ok (obj, {t with texture_cache})

let write t ?font_name ?font_size (r, g, b) ?(a = 255) ?(scale = 1.0) ?(x = 0.)
    ?(y = 0.) text =
  let* font_name, font_size = get_font_spec t font_name font_size in
  let* obj, t = get_obj t font_name font_size (r, g, b, a) text in
  Text_object.draw obj scale x y ;
  Ok t

let terminate t =
  Texture_cache.iter (fun _ obj -> Text_object.delete obj) t.texture_cache ;
  Font_cache.iter (fun _ font -> Ttf.close_font font) t.font_cache ;
  Ttf.quit ()
