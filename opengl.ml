open Tsdl
open Tgl4
open Glutils
open Result
open Globjects

type objects = {
  pawn : Pawn.t;
}

type context = {
  win : Sdl.window;
  ctx : Sdl.gl_context;
  pid : int;
  poll_state : unit -> State.t option;
  buffer_input : Input.t -> unit;
  send_inputs : unit -> unit;
  objects : objects;
}

let gl = (4,4)

let square_size = 100
let window_width = 8 * square_size
let window_height = 3 * square_size

let clear_screen ?(r=0.5) ?(g=0.5) ?(b=0.5) () =
  Gl.clear_color r g b 1.;
  Gl.clear Gl.color_buffer_bit

let reshape _win w h =
  Gl.viewport 0 0 w h

let proj_matrix = Matrix.ortho 0. 8. 0. 3. (-1.) 1.

let draw_title _game animations context =
  let open Animation in
  let ta = List.find_opt
      (fun x -> match x.kind with Title -> true | _ -> false) animations in
  begin match ta with
    | None -> clear_screen ()
    | Some t ->
      let prog = (progress t) /. 2. in
      clear_screen ~r:prog ~g:prog ~b:prog ()
  end;
  Sdl.gl_swap_window context.win

let draw_playing game animations context =
  let open Game in
  let open Animation in
  clear_screen ();
  Gl.use_program context.pid;
  let viewid = Gl.get_uniform_location context.pid "view" in
  Gl.uniform_matrix4fv viewid 1 true (Matrix.raw proj_matrix);
  let player p = if p = P1 then game.logic.p1 else game.logic.p2 in
  begin match game.gameplay with
  | Choose (p, _, choices) when (player p).p_type = Human_player ->
    List.iter (fun (pawn, _) ->
        Pawn.draw context.pid context.objects.pawn ~choice:true pawn
      ) choices
  | _ -> ()
  end;
  let normal_pawns = List.filter (fun pawn -> 
      not @@ List.exists (fun a ->
          match a.kind with
          | Pawn_moving (mp, _) when mp = pawn -> true
          | _ -> false
        ) animations
    ) game.logic.pawns in
  List.iter (Pawn.draw context.pid context.objects.pawn) normal_pawns;
  List.iter (fun a ->
      let prog = Animation.progress a in
      let d orig dest p =
        Pawn.draw context.pid context.objects.pawn ~animate:(dest, p) orig
      in match a.kind with
      | Pawn_moving (p, Move position) 
      | Pawn_moving (p, Take {position; _}) -> d p {p with position} prog
      | Pawn_moving (p, Add) ->
        d {p with position = Reserve} p (0.5 +. (prog /. 2.))
      | Pawn_moving (p, Finish) ->
        d p {p with position = Outro 2} (prog /. 2.)
      | _ -> ()
    ) animations;
  Sdl.gl_swap_window context.win

module Timer = struct
  let time = Unix.gettimeofday
  type t = {
    last : float ref;
    length : float;
  }

  let create length =
    {last = ref (time ()); length}

  let reset t = t.last := time ()
  let check t =
    if time () -. !(t.last) > t.length then (
      reset t; true
    ) else false
end

module Fps = struct
  let frames = ref 0
  let interval = 2.
  let timer = Timer.create interval

  let compute () =
    let fps = (float !frames) /. interval in
    frames := 0;
    fps

  let check () =
    incr frames;
    if Timer.check timer then (
      let fps = compute () in
      Format.printf "FPS: %.1f@." fps
    )
end

let draw_state state context =
  Fps.check ();
  let open State in
  let rec f = function
    | Playing g ->
      draw_playing g state.animations context
    | Waiting (_, k) -> f k
    | Title_screen g ->
      draw_title g state.animations context
    | _ -> ()
  in f state.kind

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
      | `Mouse_button_down when Sdl.Event.(get e mouse_button_button = Sdl.Button.left) ->
        let open Sdl.Event in
        let x = get e mouse_button_x / square_size in
        let y = get e mouse_button_y / square_size in
        let pos = Input.coord_to_pos x y in
        context.buffer_input (Input.Pawn pos)
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
    if new_state.kind = State.End then (
      Format.printf "End of display loop@.";
      Ok context
    ) else loop new_state context
  | None ->
    if should_redraw then draw_state state context;
    loop state context

let init () =
  let* _ = Sdl.init Sdl.Init.video in
  (* Enable antialiasing *)
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1 in
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 16 in
  let* win, ctx = create_window ~gl ~w:window_width ~h:window_height in
  let* pid = create_program (glsl_version gl) in
  let* _ = Sdl.gl_set_swap_interval 0 in
  let pawn = Pawn.create () in
  Gl.enable Gl.multisample;
  Ok (win, ctx, pid, pawn)

let terminate context =
  let* _ = delete_program context.pid in
  Pawn.delete context.objects.pawn;
  let* _ = destroy_window context.win context.ctx in
  Sdl.quit ();
  Ok ()

let start ~poll_state ~buffer_input ~send_inputs ~init_state =
  match begin
    let* win, ctx, pid, pawn = init () in
    let objects = {pawn} in
    let context = {win; ctx; pid; poll_state; buffer_input; send_inputs; objects} in
    let* last_context = loop init_state context in
    terminate last_context
  end with
  | Ok () -> ()
  | Error (`Msg msg) -> Sdl.log "%s@." msg
