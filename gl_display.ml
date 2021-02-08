open Tsdl
open Tgl4
open Gl_utils
open Result
open Gl_objects

let print_fps = false
let enable_vsync = true
let square_size = 150
let board_width = 8
let board_height = 3
let board_offset_x = 2
let board_offset_y = 1
let board_margin_x = 1
let board_margin_y = 1
let board_total_width = board_width + board_offset_x + board_margin_x
let board_total_height = board_height + board_offset_y + board_margin_y
let window_width = board_total_width * square_size
let window_height = board_total_height * square_size

type objects =
  { pawn: Pawn.t
  ; board: Board.t
  ; dice: Dice.t
  ; background: Background.t
  ; cup: Cup.t }

type context =
  { win: Sdl.window
  ; ctx: Sdl.gl_context
  ; poll_state: unit -> State.t option
  ; buffer_input: Input.t -> unit
  ; send_inputs: unit -> unit
  ; objects: objects
  ; text: Gl_text.t }

let gl = (4, 4)

let clear_screen context =
  Gl.clear Gl.color_buffer_bit ;
  Background.draw context.objects.background

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

let color themes =
  let open Themes in
  let colors = text_colors themes in
  function
  | `Black -> colors.base
  | `Selected -> colors.menu_selected
  | `Alert -> colors.alert
  | `Red -> colors.p1
  | `Blue -> colors.p2
  | `Random -> (Random.int 256, Random.int 256, Random.int 256)

let draw_title themes animations context =
  let* text =
    match get_animation Title animations with
    | None -> clear_screen context ; Ok context.text
    | Some t ->
        let prog = Animation.progress t in
        clear_screen context ;
        let y = (6. *. prog) -. 1. in
        let* text =
          Gl_text.write context.text
            (color themes `Black)
            ~x:3.5 ~y ~scale:3. "Royal Game of Ur"
        in
        Ok text
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_menu m themes animations context =
  let open Menu in
  let delta = float board_total_height /. (List.length m.choices |> float) in
  let with_heights =
    List.mapi
      (fun i c ->
        ( float board_total_height -. float board_offset_y
          -. ((float i +. 0.5) *. delta)
        , c ) )
      m.choices in
  let cc = Menu.get_current_choice m in
  let open Animation in
  let is_current, is_origin, progress =
    match
      List.find_opt
        (fun a -> match a.kind with Menu_move _ -> true | _ -> false)
        animations
    with
    | Some ({kind= Menu_move (s, d); _} as a) ->
        let sc = Menu.get_nth_choice m s in
        let dc = Menu.get_nth_choice m d in
        (Choice.eq dc, Choice.eq sc, Animation.progress a)
    | _ -> (Choice.eq cc, (fun _ -> false), 1.0) in
  clear_screen context ;
  let* text =
    List.fold_left
      (fun text (y, c) ->
        let isc = is_current c in
        let iso = is_origin c in
        let col = color themes (if isc || iso then `Selected else `Black) in
        let local_progress =
          if isc then progress else if iso then 1. -. progress else 0. in
        let scale = 1. +. (0.2 *. local_progress) in
        let* t = text in
        let x = float board_width /. 2. in
        Gl_text.write t col ~scale ~x ~y (Choice.get_text c) )
      (Ok context.text) with_heights
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_victory player themes animations context =
  let* text =
    match get_animation Victory animations with
    | None -> clear_screen context ; Ok context.text
    | Some t ->
        let prog = Animation.progress t /. 2. in
        clear_screen context ;
        let scale = if prog = 0. then 10000. else 1. /. prog in
        let msg = Format.asprintf "Winner is %a" Game.pp_player player in
        let* text =
          Gl_text.write context.text
            (color themes `Black)
            ~x:3.6 ~y:1.5 ~scale msg
        in
        Ok text
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_dices (d1, d2, d3, d4) animation context =
  ( match animation with
  | None -> Cup.draw `Empty context.objects.cup
  | Some a ->
      let prog = Animation.progress a in
      if prog < 0.1 then Cup.draw `Full context.objects.cup
      else if prog < 0.9 then Cup.draw `Fallen context.objects.cup
      else Cup.draw `Empty context.objects.cup ) ;
  let off = 1. in
  let x =
    match animation with None -> -1. | Some a -> Animation.progress a -. 2.
  in
  Dice.draw context.objects.dice ~on:d1 ~x ~y:off ;
  Dice.draw context.objects.dice ~on:d2 ~x ~y:(off +. 0.5) ;
  Dice.draw context.objects.dice ~on:d3 ~x ~y:(off +. 1.0) ;
  Dice.draw context.objects.dice ~on:d4 ~x ~y:(off +. 1.5)

let draw_playing game themes animations context =
  let open Game in
  let open Animation in
  clear_screen context ;
  Board.draw context.objects.board ;
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:(-0.3) game.logic.p1.reserve
    P1 ;
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:3.3 game.logic.p2.reserve P2 ;
  let player p = if p = P1 then game.logic.p1 else game.logic.p2 in
  (* Retrieve available choices and animation progress *)
  let choices, choice_prog =
    match game.gameplay with
    | Choose (p, dices, choices) when (player p).p_type = Human_player ->
        let choice_a = get_animation Choice animations in
        draw_dices dices choice_a context ;
        let choice_prog =
          match choice_a with
          | None -> 1.
          | Some a ->
              let prog = Animation.progress a in
              if prog < 0.9 then -1. else (prog -. 0.9) *. 10. in
        let choice_pawns = List.map (fun (pawn, _) -> pawn) choices in
        (choice_pawns, choice_prog)
    | _ ->
        Cup.draw `Full context.objects.cup ;
        ([], 1.) in
  (* Draw normal (non-hollow) non-moving pawns *)
  let normal_pawns =
    List.filter
      (fun pawn ->
        not
        @@ List.exists
             (fun a ->
               match a.kind with
               | Pawn_moving (mp, _) when mp = pawn -> true
               | _ -> false )
             animations )
      game.logic.pawns in
  let is_choice p = List.exists (( = ) p) choices in
  List.iter
    (fun pawn ->
      if is_choice pawn then
        if choice_prog <> -1. then
          let choice = (`Full, choice_prog) in
          Pawn.draw context.objects.pawn ~choice pawn
        else Pawn.draw context.objects.pawn pawn
      else Pawn.draw context.objects.pawn pawn )
    normal_pawns ;
  (* Draw hollow pawns *)
  if choice_prog <> -1. then
    List.filter (fun p -> not @@ List.exists (( = ) p) normal_pawns) choices
    |> List.iter (Pawn.draw context.objects.pawn ~choice:(`Empty, choice_prog)) ;
  (* Animate moving pawn *)
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
      | _ -> () )
    animations ;
  (* Draw score (with potential animation *)
  let s1 = Printf.sprintf "%d" game.logic.p1.points in
  let s2 = Printf.sprintf "%d" game.logic.p2.points in
  let ss1 =
    match get_animation (Score_up P1) animations with
    | None -> 1.
    | Some a -> 3. -. (2. *. Animation.progress a) in
  let ss2 =
    match get_animation (Score_up P2) animations with
    | None -> 1.
    | Some a -> 3. -. (2. *. Animation.progress a) in
  let* text =
    Gl_text.write context.text (color themes `Red) ~x:5. ~y:0.5 ~scale:ss1 s1
  in
  let* text =
    Gl_text.write text (color themes `Blue) ~x:5. ~y:2.5 ~scale:ss2 s2
  in
  (* Draw cannot move if applicable *)
  let* text =
    match
      List.find_opt
        (function {kind= Cannot_choose _; _} -> true | _ -> false)
        animations
    with
    | Some {kind= Cannot_choose dices; _} ->
        draw_dices dices None context ;
        Gl_text.write text
          (color themes `Alert)
          ~scale:0.8 ~x:(-1.0) ~y:3.5 "No move !"
    | _ -> Ok text
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

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
        draw_title state.themes state.animations context
    (* Menu *)
    | Waiting (_, Menu m, _) | Menu m ->
        draw_menu m state.themes state.animations context
    (* Computation only frames *)
    | Playing {gameplay= Play _; _} | Playing {gameplay= Replay _; _} ->
        Ok context
    (* Actual game *)
    | Waiting (_, _, Playing g) | Playing g ->
        draw_playing g state.themes state.animations context
    (* Victory screen *)
    | Waiting (_, _, Victory_screen p) | Victory_screen p ->
        draw_victory p state.themes state.animations context
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
    | `Key_down when key_scancode e = `Up ->
        context.buffer_input Input.Previous_menu
    | `Key_down when key_scancode e = `Down ->
        context.buffer_input Input.Next_menu
    | `Key_down when key_scancode e = `Left ->
        context.buffer_input Input.Previous_option
    | `Key_down when key_scancode e = `Right ->
        context.buffer_input Input.Next_option
    | `Key_down when key_scancode e = `Return ->
        context.buffer_input Input.Validate
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

let init_objects themes =
  let* pawn = Pawn.create themes proj_matrix in
  let* board = Board.create themes proj_matrix in
  let* background = Background.create themes proj_matrix in
  let* cup = Cup.create themes proj_matrix in
  let* dice = Dice.create proj_matrix in
  Ok {pawn; board; dice; background; cup}

let delete_objects {pawn; board; dice; background; cup} =
  Pawn.delete pawn ;
  Board.delete board ;
  Dice.delete dice ;
  Background.delete background ;
  Cup.delete cup

let change_theme themes context =
  delete_objects context.objects ;
  let* objects = init_objects themes in
  Result.ok {context with objects}

let rec loop state context =
  let open State in
  let should_redraw = process_events context in
  context.send_inputs () ;
  match context.poll_state () with
  | Some new_state ->
      let* context =
        if Themes.selected new_state.themes <> Themes.selected state.themes then
          change_theme new_state.themes context
        else Result.ok context
      in
      let* context = draw_state new_state context in
      if new_state.kind = End then (
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

let init init_state =
  let open Tsdl_image in
  (* TODO propre ressource cleaning in case of error *)
  let* _ = Sdl.init Sdl.Init.video in
  let img_flags = Image.Init.(jpg + png) in
  assert (Image.init img_flags = img_flags) ;
  (* Enable antialiasing *)
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1 in
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 16 in
  let* win, ctx = create_window ~gl ~w:window_width ~h:window_height in
  let* _ = Sdl.gl_set_swap_interval (if enable_vsync then 1 else 0) in
  Gl.enable Gl.blend ;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha ;
  let* text = Gl_text.init proj_matrix in
  let text = Gl_text.set_default text State.(Themes.font init_state.themes) 42 in
  let* objects = init_objects init_state.themes in
  Gl.enable Gl.multisample ;
  Ok (win, ctx, objects, text)

let terminate context =
  delete_objects context.objects ;
  let* _ = destroy_window context.win context.ctx in
  Gl_text.terminate context.text ;
  Tsdl_image.Image.quit () ;
  Sdl.quit () ;
  Ok ()

let start ~poll_state ~buffer_input ~send_inputs ~init_state ~error =
  match
    let* win, ctx, objects, text = init init_state in
    let context =
      {win; ctx; poll_state; buffer_input; send_inputs; objects; text} in
    let* last_context = loop init_state context in
    terminate last_context
  with
  | Ok () -> ()
  | Error (`Msg msg) ->
      Sdl.log_critical Sdl.Log.category_video "%s" msg ;
      error msg
