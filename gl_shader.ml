open Tgl4
open Gl_utils

let read_all_file filename =
  try
    let chan = open_in filename in
    let rec aux chan =
      try
        let line = input_line chan in
        Printf.sprintf "%s\n%s" line (aux chan)
      with End_of_file -> "" in
    let src = aux chan in
    close_in chan ; Result.ok src
  with Sys_error m -> Result.error (`Msg m)

let compile_shader filename typ =
  let* src = read_all_file filename in
  let get_shader sid e = get_int (Gl.get_shaderiv sid e) in
  let sid = Gl.create_shader typ in
  Gl.shader_source sid src ;
  Gl.compile_shader sid ;
  if get_shader sid Gl.compile_status = Gl.true_ then Ok sid
  else
    let len = get_shader sid Gl.info_log_length in
    let log = get_string len (Gl.get_shader_info_log sid len None) in
    Gl.delete_shader sid ;
    let msg =
      Printf.sprintf "Upon compiling %s: %s\nSource was : %s" filename log src
    in
    Error (`Msg msg)

type t = {pid: int}

let create ?(v_filename = "shaders/default.vert")
    ?(f_filename = "shaders/default.frag") attributes =
  let* vid = compile_shader v_filename Gl.vertex_shader in
  let* fid = compile_shader f_filename Gl.fragment_shader in
  let pid = Gl.create_program () in
  let get_program pid e = get_int (Gl.get_programiv pid e) in
  Gl.attach_shader pid vid ;
  Gl.delete_shader vid ;
  Gl.attach_shader pid fid ;
  Gl.delete_shader fid ;
  Gl.link_program pid ;
  let* () =
    if get_program pid Gl.link_status = Gl.true_ then Ok ()
    else
      let len = get_program pid Gl.info_log_length in
      let log = get_string len (Gl.get_program_info_log pid len None) in
      Gl.delete_program pid ;
      let msg =
        Printf.sprintf "Upon linking [%s | %s]: %s" v_filename f_filename log
      in
      Error (`Msg msg)
  in
  let* () =
    let with_i = List.mapi (fun i x -> (i, x)) attributes in
    List.fold_left
      (fun status (i, name) ->
        if Gl.get_attrib_location pid name = -1 then
          let msg =
            Printf.sprintf "%s is not a variable in [%s | %s]" name v_filename
              f_filename in
          Error (`Msg msg)
        else
          let* () = status in
          Ok (Gl.bind_attrib_location pid i name) )
      (Result.ok ()) with_i
  in
  Ok {pid}

let delete t = Gl.delete_program t.pid

let send_matrix t name mat =
  Gl.use_program t.pid ;
  let viewid = Gl.get_uniform_location t.pid name in
  Gl.uniform_matrix4fv viewid 1 true (Matrix.raw mat)
