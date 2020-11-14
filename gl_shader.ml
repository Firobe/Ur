open Tgl4
open Gl_utils

let glsl_v = "440"

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

type t = {pid: int}

let create () =
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
  if get_program pid Gl.link_status = Gl.true_ then Ok {pid}
  else
    let len = get_program pid Gl.info_log_length in
    let log = get_string len (Gl.get_program_info_log pid len None) in
    Gl.delete_program pid ;
    Error (`Msg log)

let delete t = Gl.delete_program t.pid

let send_matrix t name mat =
  Gl.use_program t.pid ;
  let viewid = Gl.get_uniform_location t.pid name in
  Gl.uniform_matrix4fv viewid 1 true (Matrix.raw mat)
