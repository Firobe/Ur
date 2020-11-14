open Tgl4
open Tsdl

let ( let* ) x f = match x with Ok v -> f v | Error _ as e -> e
let bigarray_create k len = Bigarray.(Array1.create k c_layout len)
let bigarray_of k a = Bigarray.(Array1.of_array k c_layout a)

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

let get_string len f =
  let a = bigarray_create Bigarray.char len in
  f a ; Gl.string_of_bigarray a

let create_buffer b =
  let id = get_int (Gl.gen_buffers 1) in
  let bytes = Gl.bigarray_byte_size b in
  Gl.bind_buffer Gl.array_buffer id ;
  Gl.buffer_data Gl.array_buffer bytes (Some b) Gl.static_draw ;
  id

let delete_buffer bid = set_int (Gl.delete_buffers 1) bid

(* Shaders *)

let glsl_version gl_version =
  match gl_version with
  | 3, 2 -> "150"
  | 3, 3 -> "330"
  | 4, 0 -> "400"
  | 4, 1 -> "410"
  | 4, 2 -> "420"
  | 4, 3 -> "430"
  | 4, 4 -> "440"
  | _ -> assert false

let str = Printf.sprintf

let vertex_shader v =
  str
    "\n\
    \  #version %s core\n\
    \  in vec3 vertex;\n\
    \  in vec3 color;\n\
    \  out vec4 v_color;\n\
    \  uniform mat4 model;\n\
    \  uniform mat4 view;\n\n\
    \  void main()\n\
    \  {\n\
    \    v_color = vec4(color, 1.0);\n\
    \    gl_Position = view * model * vec4(vertex, 1.0);\n\
    \  }" v

let fragment_shader v =
  str
    "\n\
    \  #version %s core\n\
    \  in vec4 v_color;\n\
    \  out vec4 color;\n\
    \  void main() { color = v_color; }" v

let compile_shader src typ =
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src ;
  Gl.compile_shader sid ;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid ;
    Error (`Msg log)

let create_program glsl_v =
  let* vid = compile_shader (vertex_shader glsl_v) Gl.vertex_shader in
  let* fid = compile_shader (fragment_shader glsl_v) Gl.fragment_shader in
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid ;
  Gl.delete_shader vid ;
  Gl.attach_shader pid fid ;
  Gl.delete_shader fid ;
  Gl.bind_attrib_location pid 0 "vertex" ;
  Gl.bind_attrib_location pid 1 "color" ;
  Gl.link_program pid ;
  if get_program pid Gl.link_status = Gl.true_ then Ok pid
  else
    let len = get_program pid Gl.info_log_length in
    let log = get_string len (Gl.get_program_info_log pid len None) in
    Gl.delete_program pid ;
    Error (`Msg log)

let delete_program pid = Gl.delete_program pid ; Ok ()

(* Window and OpenGL context *)

let pp_opengl_info ppf () =
  let pp = Format.fprintf in
  let pp_opt ppf = function None -> pp ppf "error" | Some s -> pp ppf "%s" s in
  pp ppf "Using OpenGL backend@." ;
  pp ppf "@[<v>@," ;
  pp ppf "Renderer @[<v>@[%a@]@," pp_opt (Gl.get_string Gl.renderer) ;
  pp ppf "@[OpenGL %a / GLSL %a@]@]@," pp_opt (Gl.get_string Gl.version) pp_opt
    (Gl.get_string Gl.shading_language_version) ;
  pp ppf "@]"

let create_window ~gl:(maj, min) ~w ~h =
  let w_atts = Sdl.Window.(opengl (*+ resizable*)) in
  let w_title = Printf.sprintf "OpenGL Game of Ur" in
  let set a v = Sdl.gl_set_attribute a v in
  let* _ = set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core in
  let* _ = set Sdl.Gl.context_major_version maj in
  let* _ = set Sdl.Gl.context_minor_version min in
  let* _ = set Sdl.Gl.doublebuffer 1 in
  let* win = Sdl.create_window ~w ~h w_title w_atts in
  let* ctx = Sdl.gl_create_context win in
  let* _ = Sdl.gl_make_current win ctx in
  Sdl.log "%a" pp_opengl_info () ;
  Ok (win, ctx)

let destroy_window win ctx =
  Sdl.gl_delete_context ctx ; Sdl.destroy_window win ; Ok ()
