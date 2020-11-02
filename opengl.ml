open Tsdl
open Tgl4
open Glutils
open Result

type context = {
  win : Sdl.window;
  ctx : Sdl.gl_context;
  pid : int;
  geometry : Geometry.t;
  poll_state : unit -> State.t option;
  buffer_input : Input.t -> unit;
  send_inputs : unit -> unit;
}

let gl = (4,4)
let window_width = 800
let window_height = 600

(* Geometry *)
let set_3d ba i x y z =
  let start = i * 3 in
  ba.{start} <- x; ba.{start + 1} <- y; ba.{start + 2} <- z

let triangle =
  (Gl.lines,
   [| -1.0; -1.0; 0.0;
      1.0; -1.0; 0.0;
      1.0;  1.0; 0.0;
      -1.0;  1.0; 0.0 |],
   [| 1.0; 0.0; 0.0;
      0.0; 1.0; 0.0;
      0.0; 0.0; 1.0;
      1.0; 0.0; 1.0 |],
   [| 0; 1; 2;
      0; 2; 3 |])

let clear_screen () =
  Gl.clear_color 0. 0. 0. 1.;
  Gl.clear Gl.color_buffer_bit

let reshape _win w h =
  Gl.viewport 0 0 w h

let draw_playing _game context =
  clear_screen ();
  Gl.use_program context.pid;
  Geometry.draw context.geometry;
  Sdl.gl_swap_window context.win

let draw_state state context = match state with
  | State.Playing g -> draw_playing g context
  | _ -> ()

let quit context =
  Format.printf "User quit.@.";
  context.buffer_input Input.Quit

let process_events context =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let should_redraw = ref false in
  while Sdl.poll_event (Some e) do
    begin match event e with
      | `Quit -> quit context
      | `Key_down when key_scancode e = `Escape -> quit context
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
  let* geometry = Geometry.of_arrays triangle in
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
