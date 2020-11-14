open Tsdl
open Tgl4
open Glutils
open Result
open Globjects

let square_size = 100
let board_width = 8
let board_height = 3
let board_offset_x = 2
let board_offset_y = 1
let board_margin_x = 1
let board_margin_y = 1
let window_width = (board_width + board_offset_x + board_margin_x) * square_size

let window_height =
  (board_height + board_offset_y + board_margin_y) * square_size

let print_fps = true

type objects = {pawn: Pawn.t; board: Board.t; dice: Dice.t}

type context =
  { win: Sdl.window
  ; ctx: Sdl.gl_context
  ; pid: int
  ; poll_state: unit -> State.t option
  ; buffer_input: Input.t -> unit
  ; send_inputs: unit -> unit
  ; objects: objects }

let gl = (4, 4)

let clear_screen ?(r = 0.5) ?(g = 0.5) ?(b = 0.5) () =
  Gl.clear_color r g b 1. ;
  Gl.clear Gl.color_buffer_bit

let reshape _win w h = Gl.viewport 0 0 w h

(* For the board *)
let proj_matrix =
  Matrix.ortho (float (-board_offset_x))
    (float (board_width + board_margin_x))
    (float (-board_offset_y))
    (float (board_height + board_margin_y))
    (-1.) 1.

let get_animation kind =
  let open Animation in
  List.find_opt (fun x -> x.kind = kind)

let draw_title animations context =
  ( match get_animation Title animations with
  | None -> clear_screen ()
  | Some t ->
      let prog = Animation.progress t /. 2. in
      clear_screen ~r:prog ~g:prog ~b:prog () ) ;
  Sdl.gl_swap_window context.win

let draw_victory player animations context =
  ( match get_animation Victory animations with
  | None -> clear_screen ()
  | Some t ->
      let prog = Animation.progress t /. 2. in
      let r = if player = Game.P1 then 0.5 +. prog else 0.5 -. prog in
      let b = if player = Game.P2 then 0.5 +. prog else 0.5 -. prog in
      clear_screen ~r ~g:(0.5 -. prog) ~b () ) ;
  Sdl.gl_swap_window context.win

let draw_dices (d1, d2, d3, d4) animation context =
  let x =
    match animation with None -> -1. | Some a -> -2. +. Animation.progress a
  in
  let off = 1. in
  Dice.draw context.pid context.objects.dice ~on:d1 ~x ~y:off ;
  Dice.draw context.pid context.objects.dice ~on:d2 ~x ~y:(off +. 0.5) ;
  Dice.draw context.pid context.objects.dice ~on:d3 ~x ~y:(off +. 1.0) ;
  Dice.draw context.pid context.objects.dice ~on:d4 ~x ~y:(off +. 1.5)

let draw_playing game animations context =
  let open Game in
  let open Animation in
  clear_screen () ;
  Gl.use_program context.pid ;
  let viewid = Gl.get_uniform_location context.pid "view" in
  Gl.uniform_matrix4fv viewid 1 true (Matrix.raw proj_matrix) ;
  Board.draw context.pid context.objects.board ;
  Pawn.draw_reserve context.pid context.objects.pawn ~x:0.2 ~y:(-0.3)
    game.logic.p1.reserve P1 ;
  Pawn.draw_reserve context.pid context.objects.pawn ~x:0.2 ~y:3.3
    game.logic.p2.reserve P2 ;
  let player p = if p = P1 then game.logic.p1 else game.logic.p2 in
  ( match game.gameplay with
  | Choose (p, dices, choices) when (player p).p_type = Human_player ->
      let choice_a = get_animation Choice animations in
      draw_dices dices choice_a context ;
      List.iter
        (fun (pawn, _) ->
          let choice =
            match choice_a with None -> 1. | Some a -> Animation.progress a
          in
          Pawn.draw context.pid context.objects.pawn ~choice pawn)
        choices
  | _ -> () ) ;
  let normal_pawns =
    List.filter
      (fun pawn ->
        not
        @@ List.exists
             (fun a ->
               match a.kind with
               | Pawn_moving (mp, _) when mp = pawn -> true
               | _ -> false)
             animations)
      game.logic.pawns in
  List.iter (Pawn.draw context.pid context.objects.pawn) normal_pawns ;
  List.iter
    (fun a ->
      let prog = Animation.progress a in
      let d orig dest p =
        Pawn.draw context.pid context.objects.pawn ~animate:(dest, p) orig in
      match a.kind with
      | Pawn_moving (p, Move position) | Pawn_moving (p, Take {position; _}) ->
          d p {p with position} prog
      | Pawn_moving (p, Add) -> d {p with position= Reserve} p prog
      | Pawn_moving (p, Finish) -> d p {p with position= Outro 2} prog
      | _ -> ())
    animations ;
  Sdl.gl_swap_window context.win

module Timer = struct
  let time = Unix.gettimeofday

  type t = {last: float ref; length: float}

  let create length = {last= ref (time ()); length}
  let reset t = t.last := time ()

  let check t =
    if time () -. !(t.last) > t.length then (reset t ; true) else false
end

module Fps = struct
  let frames = ref 0
  let interval = 5.
  let timer = Timer.create interval

  let compute () =
    let fps = float !frames /. interval in
    frames := 0 ;
    fps

  let check () =
    incr frames ;
    if Timer.check timer then
      let fps = compute () in
      Format.printf "FPS: %.1f@." fps
end

let draw_state state context =
  if print_fps then Fps.check () ;
  let open State in
  let f = function
    (* Title screen *)
    | Waiting (_, Title_screen, _) | Title_screen ->
        draw_title state.animations context
    (* Computation only frames *)
    | Playing {gameplay= Play _; _} | Playing {gameplay= Replay _; _} -> ()
    (* Actual game *)
    | Waiting (_, _, Playing g) | Playing g ->
        draw_playing g state.animations context
    (* Victory screen *)
    | Waiting (_, _, Victory_screen p) | Victory_screen p ->
        draw_victory p state.animations context
    | _ -> () in
  f state.kind

let quit context =
  Format.printf "User quit.@." ;
  context.buffer_input Input.Quit

let process_events context =
  let e = Sdl.Event.create () in
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let should_redraw = ref false in
  while Sdl.poll_event (Some e) do
    match event e with
    | `Mouse_button_down
      when Sdl.Event.(get e mouse_button_button = Sdl.Button.left) ->
        let open Sdl.Event in
        let x = (get e mouse_button_x / square_size) - board_offset_x in
        let y = (get e mouse_button_y / square_size) - board_offset_y in
        let pos = Input.coord_to_pos x y in
        context.buffer_input (Input.Pawn pos)
    | `Quit -> quit context
    | `Key_down when key_scancode e = `Escape -> quit context
    | `Window_event -> (
      match window_event e with
      | `Exposed | `Resized ->
          let w, h = Sdl.get_window_size context.win in
          reshape context.win w h ;
          should_redraw := true
      | _ -> () )
    | _ -> ()
  done ;
  !should_redraw

let rec loop state context =
  let should_redraw = process_events context in
  context.send_inputs () ;
  match context.poll_state () with
  | Some new_state ->
      draw_state new_state context ;
      if new_state.kind = State.End then (
        Format.printf "End of display loop@." ;
        Ok context )
      else loop new_state context
  | None ->
      if should_redraw then draw_state state context ;
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
  let board = Board.create () in
  let dice = Dice.create () in
  Gl.enable Gl.multisample ;
  Ok (win, ctx, pid, {pawn; board; dice})

let terminate context =
  let* _ = delete_program context.pid in
  Pawn.delete context.objects.pawn ;
  let* _ = destroy_window context.win context.ctx in
  Sdl.quit () ; Ok ()

let start ~poll_state ~buffer_input ~send_inputs ~init_state =
  match
    let* win, ctx, pid, objects = init () in
    let context =
      {win; ctx; pid; poll_state; buffer_input; send_inputs; objects} in
    let* last_context = loop init_state context in
    terminate last_context
  with
  | Ok () -> ()
  | Error (`Msg msg) -> Sdl.log "%s@." msg
