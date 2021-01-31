open Tsdl
open Tgl4
open Gl_utils
open Result
open Gl_objects

let print_fps = true
let enable_vsync = true
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

type objects = {pawn: Pawn.t; board: Board.t; dice: Dice.t}

type context =
  { win: Sdl.window
  ; ctx: Sdl.gl_context
  ; poll_state: unit -> State.t option
  ; buffer_input: Input.t -> unit
  ; send_inputs: unit -> unit
  ; objects: objects
  ; text: Gl_text.t }

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

let black = Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255

let draw_title animations context =
  let* text =
    match get_animation Title animations with
    | None -> clear_screen () ; Ok context.text
    | Some t ->
        let* text = Gl_text.write context.text black "Game of Ur!" in
        let prog = Animation.progress t /. 2. in
        clear_screen ~r:prog ~g:prog ~b:prog () ;
        Ok text
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_victory player animations context =
  let* text =
    match get_animation Victory animations with
    | None -> clear_screen () ; Ok context.text
    | Some t ->
        let* text = Gl_text.write context.text black "OULALA" in
        let prog = Animation.progress t /. 2. in
        let r = if player = Game.P1 then 0.5 +. prog else 0.5 -. prog in
        let b = if player = Game.P2 then 0.5 +. prog else 0.5 -. prog in
        clear_screen ~r ~g:(0.5 -. prog) ~b () ;
        Ok text
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_dices (d1, d2, d3, d4) animation context =
  let x =
    match animation with None -> -1. | Some a -> -2. +. Animation.progress a
  in
  let off = 1. in
  Dice.draw context.objects.dice ~on:d1 ~x ~y:off ;
  Dice.draw context.objects.dice ~on:d2 ~x ~y:(off +. 0.5) ;
  Dice.draw context.objects.dice ~on:d3 ~x ~y:(off +. 1.0) ;
  Dice.draw context.objects.dice ~on:d4 ~x ~y:(off +. 1.5)

let draw_playing game animations context =
  let open Game in
  let open Animation in
  clear_screen () ;
  Board.draw context.objects.board ;
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:(-0.3) game.logic.p1.reserve
    P1 ;
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:3.3 game.logic.p2.reserve P2 ;
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
          Pawn.draw context.objects.pawn ~choice pawn)
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
  List.iter (Pawn.draw context.objects.pawn) normal_pawns ;
  List.iter
    (fun a ->
      let prog = Animation.progress a in
      let d orig dest p =
        Pawn.draw context.objects.pawn ~animate:(dest, p) orig in
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
    | Playing {gameplay= Play _; _} | Playing {gameplay= Replay _; _} ->
        Ok context
    (* Actual game *)
    | Waiting (_, _, Playing g) | Playing g ->
        draw_playing g state.animations context ;
        Ok context
    (* Victory screen *)
    | Waiting (_, _, Victory_screen p) | Victory_screen p ->
        draw_victory p state.animations context
    | _ -> Ok context in
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
      let* context = draw_state new_state context in
      if new_state.kind = State.End then (
        Format.printf "End of display loop@." ;
        Ok context )
      else loop new_state context
  | None ->
      let* context =
        if should_redraw then draw_state state context else Ok context
      in
      loop state context

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

let init () =
  (* TODO propre ressource cleaning in case of error *)
  let* _ = Sdl.init Sdl.Init.video in
  (* Enable antialiasing *)
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1 in
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 16 in
  let* win, ctx = create_window ~gl ~w:window_width ~h:window_height in
  let* _ = Sdl.gl_set_swap_interval (if enable_vsync then 1 else 0) in
  let* text = Gl_text.init () in
  let text = Gl_text.set_default text "Compagnon-Light.otf" 42 in
  let* pawn = Pawn.create proj_matrix in
  let* board = Board.create proj_matrix in
  let* dice = Dice.create proj_matrix in
  Gl.enable Gl.multisample ;
  Ok (win, ctx, {pawn; board; dice}, text)

let terminate context =
  Pawn.delete context.objects.pawn ;
  let* _ = destroy_window context.win context.ctx in
  Gl_text.terminate context.text ;
  Sdl.quit () ;
  Ok ()

let start ~poll_state ~buffer_input ~send_inputs ~init_state ~error =
  match
    let* win, ctx, objects, text = init () in
    let context =
      {win; ctx; poll_state; buffer_input; send_inputs; objects; text} in
    let* last_context = loop init_state context in
    terminate last_context
  with
  | Ok () -> ()
  | Error (`Msg msg) ->
    Sdl.log_critical Sdl.Log.category_video "%s" msg ;
    error msg
