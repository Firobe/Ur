open Tsdl
open Tgl4
open Glutils
open Result

let gl = (4,4)
let window_width = 800
let window_height = 600

(* Geometry *)
let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x; ba.{start + 1} <- y; ba.{start + 2} <- z

let triangle = {
  Geometry.vertices = (
    let vs = bigarray_create Bigarray.float32 (3 * 3) in
    set_3d vs 0 (-1.0) (-1.0) 0.0;
    set_3d vs 1 1.0    (-1.0) 0.0;
    set_3d vs 2 0.0    1.0    0.0;
    vs);
  colors = (
    let cs = bigarray_create Bigarray.float32 (3 * 3) in
    set_3d cs 0 1.0 0.0 0.0;
    set_3d cs 1 0.0 1.0 0.0;
    set_3d cs 2 0.0 0.0 1.0;
    cs);
  indices = (
    let is = bigarray_create Bigarray.int8_unsigned 3 in
    set_3d is 0 0 1 2;
    is)
}

(* OpenGL setup *)
let draw pid gid win =
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit;
  Gl.use_program pid;
  Gl.bind_vertex_array gid;
  Gl.draw_elements Gl.triangles 3 Gl.unsigned_byte (`Offset 0);
  Gl.bind_vertex_array 0;
  Sdl.gl_swap_window win

let reshape _win w h =
  Gl.viewport 0 0 w h

(* Main *)

type context = {
  win : Sdl.window;
  ctx : Sdl.gl_context;
  pid : int;
  geometry : Geometry.t;
  poll_state : unit -> State.t option;
  buffer_input : Input.t -> unit;
  send_inputs : unit -> unit;
}

let process_events context =
  let e = Sdl.Event.create () in
  (* let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in *)
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let should_redraw = ref false in
  while Sdl.poll_event (Some e) do
    begin match event e with
      | `Quit ->
        Format.printf "User quit.@.";
        context.buffer_input Input.Quit
      (*| `Key_down when key_scancode e = `Escape -> Ok () *)
      | `Window_event ->
        begin match window_event e with
          | `Exposed | `Resized ->
            let w, h = Sdl.get_window_size context.win in
            reshape context.win w h;
            should_redraw := true;
          | _ -> ()
        end
      | _ -> ()
    end
  done;
  !should_redraw

let draw_playing _game context =
  draw context.pid context.geometry.gid context.win

let draw_state state context = match state with
  | State.Playing g -> draw_playing g context
  | _ -> ()

let rec loop state context =
  let should_redraw = process_events context in
  context.send_inputs ();
  match context.poll_state () with
  | Some new_state ->
    draw_state new_state context;
    if new_state = State.End then (
      Format.printf "End of display loop@.";
      Ok context
    ) else loop new_state context
  | None ->
    if should_redraw then draw_state state context;
    loop state context

let init () =
  let* _ = Sdl.init Sdl.Init.video in
  let* win, ctx = create_window ~gl ~w:window_width ~h:window_height in
  let* pid = create_program (glsl_version gl) in
  let* geometry = Geometry.create triangle in
  Ok (win, ctx, pid, geometry)

let terminate context =
  let* _ = delete_program context.pid in
  let* _ = Geometry.delete context.geometry in
  let* _ = destroy_window context.win context.ctx in
  Sdl.quit ();
  Ok ()

let start ~poll_state ~buffer_input ~send_inputs ~init_state =
  match begin
    let* win, ctx, pid, geometry = init () in
    let context = {win; ctx; pid; geometry; poll_state; buffer_input; send_inputs} in
    let* last_context = loop init_state context in
    terminate last_context
  end with
  | Ok () -> ()
  | Error (`Msg msg) -> Sdl.log "%s@." msg
