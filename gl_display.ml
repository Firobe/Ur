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
  ; arrows: Arrows.t
  ; dice: Dice.t
  ; background: Background.t
  ; cup: Cup.t }

type dice_data = {kinds: int list; coords: (float * float) list}

type context =
  { win: Sdl.window
  ; ctx: Sdl.gl_context
  ; poll_state: unit -> State.t option
  ; buffer_input: Input.t -> unit
  ; send_inputs: unit -> unit
  ; objects: objects
  ; dice_data: dice_data option
  ; sounds: Gl_audio.t
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
  let colors = text_styles themes in
  function
  | `Black ->
      colors.base
  | `Selected ->
      colors.menu_selected
  | `Alert ->
      colors.alert
  | `Red ->
      colors.p1
  | `Blue ->
      colors.p2
  | `Random ->
      {color= (Random.int 256, Random.int 256, Random.int 256); outline= None}

let draw_title themes animations context =
  let* context =
    match get_animation Title animations with
    | None ->
        clear_screen context ; Ok context
    | Some t ->
        let* sounds =
          Gl_audio.play_theme context.sounds ~anim_unique:t `title
        in
        let prog = Animation.progress t in
        clear_screen context ;
        let y = (6. *. prog) -. 1. in
        let* text =
          Gl_text.write context.text
            (color themes `Black)
            ~x:3.5 ~y ~scale:3. "Royal Game of Ur"
        in
        Ok {context with text; sounds}
  in
  Sdl.gl_swap_window context.win ;
  Result.ok context

let play_animation_sound animations context kind =
  match get_animation (Sound kind) animations with
  | None ->
      Ok context
  | Some a ->
      let* sounds = Gl_audio.play_theme ~anim_unique:a context.sounds kind in
      Result.ok {context with sounds}

let draw_menu m themes animations context =
  let open Menu in
  let delta = float board_total_height /. (List.length m.choices |> float) in
  let with_heights =
    List.mapi
      (fun i c ->
        ( float board_total_height -. float board_offset_y
          -. ((float i +. 0.5) *. delta)
        , c ) )
      m.choices
  in
  let cc = Menu.get_current_choice m in
  let open Animation in
  let* is_current, is_origin, progress, context =
    match
      List.find_opt
        (fun a -> match a.kind with Menu_move _ -> true | _ -> false)
        animations
    with
    | Some ({kind= Menu_move (s, d); _} as a) ->
        let* sounds =
          Gl_audio.play_theme ~anim_unique:a context.sounds `menu_choice
        in
        let sc = Menu.get_nth_choice m s in
        let dc = Menu.get_nth_choice m d in
        Result.ok
          ( Choice.eq dc
          , Choice.eq sc
          , Animation.progress a
          , {context with sounds} )
    | _ ->
        Result.ok (Choice.eq cc, (fun _ -> false), 1.0, context)
  in
  let* context = play_animation_sound animations context `menu_option in
  clear_screen context ;
  let* text =
    List.fold_left
      (fun text (y, c) ->
        let isc = is_current c in
        let iso = is_origin c in
        let col = color themes (if isc || iso then `Selected else `Black) in
        let local_progress =
          if isc then progress else if iso then 1. -. progress else 0.
        in
        let scale = 1. +. (0.2 *. local_progress) in
        let* t = text in
        let x = float board_width /. 2. in
        Gl_text.write t col ~scale ~x ~y (Choice.get_text c) )
      (Ok context.text) with_heights
  in
  Sdl.gl_swap_window context.win ;
  Ok {context with text}

let draw_victory player themes animations context =
  let* context =
    match get_animation Victory animations with
    | None ->
        clear_screen context ; Ok context
    | Some t ->
        let* sounds =
          Gl_audio.play_theme ~anim_unique:t context.sounds `victory
        in
        let prog = Animation.progress t /. 2. in
        clear_screen context ;
        let scale = if prog = 0. then 10000. else 1. /. prog in
        let msg = Format.asprintf "Winner is %a" Game.pp_player player in
        let* text =
          Gl_text.write context.text
            (color themes `Black)
            ~x:3.6 ~y:1.5 ~scale msg
        in
        Ok {context with text; sounds}
  in
  Sdl.gl_swap_window context.win ;
  Ok context

let throw_dice_data () =
  let throw () =
    let r = Random.bool () in
    1 + if r then 1 else 0
  in
  let dns = [throw (); throw (); throw (); throw ()] in
  let grid_x = 3 in
  let grid_y = 4 in
  let throw_coord () =
    let x = Random.int grid_x in
    let y = Random.int grid_y in
    (x, y)
  in
  let already_chosen c l = List.exists (( = ) c) l in
  let rec choose_unique l =
    let r = throw_coord () in
    if already_chosen r l then choose_unique l else r
  in
  let icoords =
    List.fold_left (fun acc () -> choose_unique acc :: acc) [] [(); (); (); ()]
  in
  let sorted =
    List.sort (fun (_, y1) (_, y2) -> Stdlib.compare y2 y1) icoords
  in
  let to_float_coords (x, y) =
    let x = ((0. +. 1.5) /. float grid_x *. float x) -. 1.5 in
    let y = ((3.0 -. 1.0) /. float grid_y *. float y) +. 1.0 in
    (x, y)
  in
  let real = List.map to_float_coords sorted in
  {kinds= dns; coords= real}

let write_in_corner ?(off = 0.) context themes msg =
  let* text =
    Gl_text.write context.text
      (color themes `Alert)
      ~scale:0.8 ~x:(-1.0) ~y:(3.5 +. off) msg
  in
  Result.ok {context with text}

let draw_dices (d1, d2, d3, d4) animation context themes =
  (* Draw dices *)
  let kinds, coords, context =
    match context.dice_data with
    | None ->
        let data = throw_dice_data () in
        (data.kinds, data.coords, {context with dice_data= Some data})
    | Some {kinds; coords} ->
        (kinds, coords, context)
  in
  let prog =
    match animation with None -> 1. | Some a -> Animation.progress a
  in
  let interpolate prog (ox, oy) (x, y) =
    let fx = ox +. ((x -. ox) *. prog) in
    let fy = oy +. ((y -. oy) *. prog) in
    (fx, fy)
  in
  let kinds_and_coords = List.map2 (fun a b -> (a, b)) kinds coords in
  List.iter2
    (fun (dice_n, (x, y)) n ->
      let x, y = interpolate prog (-1.5, -1.) (x, y) in
      Dice.draw context.objects.dice ~n ~dice_n ~x ~y )
    kinds_and_coords [d1; d2; d3; d4] ;
  if prog >= 1. then
    let sum = Game.Logic.get_dice_sum (d1, d2, d3, d4) in
    write_in_corner context themes (Printf.sprintf "Got %d" sum)
  else Result.ok context

let draw_score game animations context themes =
  let open Game in
  let s1 = Printf.sprintf "%d" game.logic.p1.points in
  let s2 = Printf.sprintf "%d" game.logic.p2.points in
  let ss1 =
    match get_animation (Score_up P1) animations with
    | None ->
        1.
    | Some a ->
        3. -. (2. *. Animation.progress a)
  in
  let ss2 =
    match get_animation (Score_up P2) animations with
    | None ->
        1.
    | Some a ->
        3. -. (2. *. Animation.progress a)
  in
  let* text =
    Gl_text.write context.text (color themes `Red) ~x:5. ~y:0.5 ~scale:ss1 s1
  in
  let* text =
    Gl_text.write text (color themes `Blue) ~x:5. ~y:2.5 ~scale:ss2 s2
  in
  Result.ok {context with text}

let maybe_draw_cannot_move animations context themes =
  match
    List.find_opt
      (function Animation.{kind= Cannot_choose _; _} -> true | _ -> false)
      animations
  with
  | Some ({kind= Cannot_choose dices; _} as a) ->
      let* context = draw_dices dices None context themes in
      let* context = write_in_corner ~off:(-0.5) context themes "No move!" in
      let* sounds =
        Gl_audio.play_theme ~anim_unique:a context.sounds `no_move
      in
      Result.ok {context with sounds}
  | _ ->
      Result.ok context

let draw_pawns context animations game (choices, choice_prog) =
  let open Game in
  let open Animation in
  (* Draw normal (non-hollow) non-moving pawns *)
  let normal_pawns =
    List.filter
      (fun pawn ->
        not
        @@ List.exists
             (fun a ->
               match a.kind with
               | Pawn_moving (mp, _) when mp = pawn ->
                   true
               | _ ->
                   false )
             animations )
      game.logic.pawns
  in
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
    |> List.iter (Pawn.draw context.objects.pawn ~choice:(`Empty, choice_prog))

let retrieve_choice_state game animations context =
  let open Game in
  match game.gameplay with
  | Choose (_, _, choices) ->
      let choice_a = get_animation Choice animations in
      let choice_prog =
        match choice_a with
        | None ->
            1.
        | Some a ->
            let prog = Animation.progress a in
            if prog < 0.9 then -1. else (prog -. 0.9) *. 10.
      in
      let choice_pawns = List.map (fun (pawn, _) -> pawn) choices in
      Result.ok (choice_pawns, choice_prog, context)
  | _ ->
      Result.ok ([], 1., context)

let draw_moving_pawn animations context =
  List.fold_left
    (fun acc a ->
      let* context = acc in
      let prog = Animation.progress a in
      let d orig dest p =
        Pawn.draw context.objects.pawn ~animate:(dest, p) orig
      in
      match a.kind with
      | Pawn_moving (p, Move position) ->
          d p {p with position} prog ;
          let* sounds =
            Gl_audio.play_theme ~anim_unique:a context.sounds `moving
          in
          Result.ok {context with sounds}
      | Pawn_moving (p, Take {position; _}) ->
          d p {p with position} prog ;
          let* sounds =
            Gl_audio.play_theme ~anim_unique:a context.sounds `eaten
          in
          Result.ok {context with sounds}
      | Pawn_moving (p, Add) ->
          d {p with position= Reserve} p prog ;
          let* sounds =
            Gl_audio.play_theme ~anim_unique:a context.sounds `spawn
          in
          Result.ok {context with sounds}
      | Pawn_moving (p, Finish) ->
          d p {p with position= Outro 2} prog ;
          let* sounds =
            Gl_audio.play_theme ~anim_unique:a context.sounds `moving
          in
          Result.ok {context with sounds}
      | _ ->
          Result.ok context )
    (Result.ok context) animations

let currently_cannot_move animations =
  List.exists
    (function Animation.{kind= Cannot_choose _; _} -> true | _ -> false)
    animations

let should_reset_dice_data game animations =
  let open Game in
  match game.gameplay with
  | Choose _ ->
      false
  | _ ->
      (* Retain dice data when showing no move *)
      not @@ currently_cannot_move animations

let draw_reserve context s1 s2 =
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:(-0.3) s1 P1 ;
  Pawn.draw_reserve context.objects.pawn ~x:0.2 ~y:3.3 s2 P2

let draw_board context game animations =
  let open Game in
  Board.draw context.objects.board ;
  draw_reserve context game.logic.p1.reserve game.logic.p2.reserve ;
  (* Retrieve available choices and animation progress *)
  let* choices, choice_prog, context =
    retrieve_choice_state game animations context
  in
  (* Draw static pawns *)
  draw_pawns context animations game (choices, choice_prog) ;
  (* Animate moving pawn *)
  draw_moving_pawn animations context

let maybe_draw_dices game animations context themes =
  let open Game in
  match game.gameplay with
  | Choose (_, dices, _) ->
      let choice_a = get_animation Choice animations in
      draw_dices dices choice_a context themes
  | _ ->
      Result.ok context

let draw_cup game animations context =
  let open Game in
  match game.gameplay with
  | Choose _ -> (
    match get_animation Choice animations with
    | None ->
        Cup.draw `Empty context.objects.cup ;
        Ok context
    | Some a ->
        let prog = Animation.progress a in
        if prog < 0.1 then (
          Cup.draw `Full context.objects.cup ;
          Ok context )
        else if prog < 0.9 then (
          let* sounds =
            Gl_audio.play_theme ~anim_unique:a context.sounds `cup_thrown
          in
          Cup.draw `Fallen context.objects.cup ;
          Result.ok {context with sounds} )
        else (
          Cup.draw `Empty context.objects.cup ;
          Ok context ) )
  | (Replay _ | Begin_turn _) when not @@ currently_cannot_move animations ->
      Cup.draw `Full context.objects.cup ;
      play_animation_sound animations context `cup_full
  | _ ->
      Cup.draw `Empty context.objects.cup ;
      Ok context

let draw_current_player game animations context themes =
  let open Game in
  match game.gameplay with
  | Begin_turn p when not @@ currently_cannot_move animations ->
      write_in_corner context themes
        (Format.asprintf "Go, %a!" Game.pp_player p)
  | Replay (p, _) when not @@ currently_cannot_move animations ->
      write_in_corner context themes
        (Format.asprintf "Again, %a!" Game.pp_player p)
  | _ ->
      Result.ok context

let draw_rules page themes animations context =
  clear_screen context ;
  (* Play select sound if coming from menu *)
  let* context = play_animation_sound animations context `select in
  let* context = play_animation_sound animations context `menu_option in
  let title, elements, sentences = Rules.get_page page in
  let text = context.text in
  (* Display page elements *)
  List.iter
    (function
      | `Board ->
          Board.draw context.objects.board
      | `Arrows ->
          Arrows.draw context.objects.arrows
      | `Cup ->
          Cup.draw `Full context.objects.cup
      | `Reserve (s1, s2) ->
          draw_reserve context s1 s2
      | `Pawn p ->
          Pawn.draw context.objects.pawn p
      | `Empty_pawn p ->
          Pawn.draw context.objects.pawn ~choice:(`Empty, 1.) p
      | `Full_pawn p ->
          Pawn.draw context.objects.pawn ~choice:(`Full, 1.) p )
    elements ;
  (* Pre-compute and display title and sentences *)
  let header = Printf.sprintf "< %s >" title in
  let* text = Gl_text.write text (color themes `Alert) ~x:3.6 ~y:3.5 header in
  let sentences = List.mapi (fun i x -> (i, x)) sentences in
  let delta = 4. /. (float @@ List.length sentences) in
  let* text =
    List.fold_left
      (fun acc (i, line) ->
        let y = -0.5 +. (float i *. delta) in
        let* text = acc in
        Gl_text.write text (color themes `Black) ~scale:delta ~x:3.6 ~y line )
      (Result.ok text) sentences
  in
  Sdl.gl_swap_window context.win ;
  Result.ok {context with text}

let draw_playing game themes animations context =
  clear_screen context ;
  (* Test if dice data should be reset *)
  let context =
    if should_reset_dice_data game animations then {context with dice_data= None}
    else context
  in
  (* Play select sound if coming from menu *)
  let* context = play_animation_sound animations context `select in
  (* Draw whole board *)
  let* context = draw_board context game animations in
  (* Draw dices when applicable *)
  let* context = maybe_draw_dices game animations context themes in
  (* Draw up with correct state *)
  let* context = draw_cup game animations context in
  (* Announce who is playing *)
  let* context = draw_current_player game animations context themes in
  (* Draw score (with potential animation *)
  let* context = draw_score game animations context themes in
  (* Draw cannot move if applicable *)
  let* context = maybe_draw_cannot_move animations context themes in
  Sdl.gl_swap_window context.win ;
  Result.ok context

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
    | Waiting (_, Title_menu m, _)
    | Title_menu m
    | Waiting (_, Pause_menu (m, _), _)
    | Pause_menu (m, _) ->
        draw_menu m state.themes state.animations context
    | Read_rules (page, _) ->
        draw_rules page state.themes state.animations context
    (* Computation only frames *)
    | Playing {gameplay= Play _; _} ->
        Result.ok context
    (* Actual game *)
    | Waiting (_, _, Playing g) | Playing g ->
        draw_playing g state.themes state.animations context
    (* Victory screen *)
    | Waiting (_, _, Victory_screen p) | Victory_screen p ->
        draw_victory p state.themes state.animations context
    (* Non-existent waiting combinations *)
    | Waiting (_, _, _) ->
        Result.ok context
    (* Game is going to shut down *)
    | End ->
        Result.ok context
  in
  f state.kind

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
        let rx = get e mouse_button_x in
        let ry = get e mouse_button_y in
        let x, y =
          Input.coord_of_raw square_size window_height board_offset_x
            board_offset_y rx ry
        in
        if Input.coord_in_board x y then
          let pos = Input.coord_to_pos x y in
          context.buffer_input (Input.Pawn pos)
        else if Input.coord_in_cup x y then
          context.buffer_input Input.Throw_dices
    | `Quit ->
        context.buffer_input Input.Quit
    | `Key_down when key_scancode e = `Escape ->
        context.buffer_input Input.Quit
    | `Key_down when key_scancode e = `Space ->
        context.buffer_input Input.Throw_dices
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
      | _ ->
          () )
    | _ ->
        ()
  done ;
  !should_redraw

let init_objects themes =
  let* pawn = Pawn.create themes proj_matrix in
  let* board = Board.create themes proj_matrix in
  let* background = Background.create themes proj_matrix in
  let* cup = Cup.create themes proj_matrix in
  let* dice = Dice.create themes proj_matrix in
  let* arrows = Arrows.create themes proj_matrix in
  Ok {pawn; board; dice; background; cup; arrows}

let delete_objects {pawn; board; dice; background; cup; arrows} =
  Pawn.delete pawn ;
  Board.delete board ;
  Dice.delete dice ;
  Background.delete background ;
  Cup.delete cup ;
  Arrows.delete arrows

let change_theme themes context =
  delete_objects context.objects ;
  Gl_audio.delete_sounds context.sounds ;
  let* sounds = Gl_audio.load_theme themes in
  let* objects = init_objects themes in
  let* font = Themes.font themes in
  let text = Gl_text.set_default context.text font 42 in
  Result.ok {context with objects; text; sounds}

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
  let pp_opt ppf = function
    | None ->
        pp ppf "error"
    | Some s ->
        pp ppf "%s" s
  in
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

let terminate_video win ctx text =
  let* _ = destroy_window win ctx in
  Gl_text.terminate text ;
  Gl_audio.terminate () ;
  Gl_texture.terminate () ;
  Sdl.quit () ;
  Ok ()

let init init_state =
  let* _ = Sdl.init Sdl.Init.(video + audio) in
  let* _ = Gl_texture.init () in
  let* _ = Gl_audio.init () in
  (* Enable antialiasing *)
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplebuffers 1 in
  let* _ = Sdl.gl_set_attribute Sdl.Gl.multisamplesamples 16 in
  let* win, ctx = create_window ~gl ~w:window_width ~h:window_height in
  let* _ = Sdl.gl_set_swap_interval (if enable_vsync then 1 else 0) in
  Gl.enable Gl.blend ;
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha ;
  let open State in
  let* text = Gl_text.init proj_matrix init_state.themes.data_path in
  let* font = Themes.font init_state.themes in
  let text = Gl_text.set_default text font 42 in
  let clean_guard = function
    | Ok o ->
        Ok o
    | Error (`Msg e) ->
        let* () = terminate_video win ctx text in
        Error (`Msg e)
  in
  let* objects = clean_guard @@ init_objects init_state.themes in
  let* sounds = clean_guard @@ Gl_audio.load_theme init_state.themes in
  Gl.enable Gl.multisample ;
  Ok (win, ctx, objects, text, sounds)

let terminate context =
  delete_objects context.objects ;
  Gl_audio.delete_sounds context.sounds ;
  terminate_video context.win context.ctx context.text

let start ~poll_state ~buffer_input ~send_inputs ~init_state ~error =
  match
    let* win, ctx, objects, text, sounds = init init_state in
    match
      let dice_data = None in
      let context =
        { win
        ; ctx
        ; poll_state
        ; buffer_input
        ; send_inputs
        ; objects
        ; text
        ; sounds
        ; dice_data }
      in
      let* last_context = loop init_state context in
      terminate last_context
    with
    | Ok () ->
        Ok ()
    | Error (`Msg msg) ->
        Sdl.log_critical Sdl.Log.category_video "%s" msg ;
        Ok (error ~at_init:false msg)
  with
  | Ok () ->
      ()
  | Error (`Msg msg) ->
      Sdl.log_critical Sdl.Log.category_video "INIT: %s" msg ;
      error ~at_init:true msg
