open Game
open Game.Gameplay

let move_time = 0.6

let title_time = 1.5

let victory_time = 5.0

let choice_time = 1.0

let menu_move_time = 0.1

let cannot_choose_time = 2.0

let score_up_time = 0.5

let debug = false

type kind =
  | Title_screen
  | Title_menu of Menu.t
  | Read_rules of int * Menu.t
  | Playing of Game.t
  | Pause_menu of Menu.t * kind
  | Waiting of int * kind * kind
  | Victory_screen of playerNo
  | End

type t =
  {kind: kind; animations: Animation.t list; speed: float; themes: Themes.t}

let is_same_kind k1 k2 =
  match (k1, k2) with
  | Playing g1, Playing g2 ->
      Game.Gameplay.is_similar_state g1.gameplay g2.gameplay
  | Waiting _, Waiting _ ->
      true
  | Title_screen, Title_screen ->
      true
  | Victory_screen _, Victory_screen _ ->
      true
  | End, End ->
      true
  | _ ->
      false

let pp fmt t =
  let fp = Format.fprintf in
  fp fmt "{animations[%d]; kind=%a}" (List.length t.animations)
    (fun fmt -> function
      | Title_screen ->
          fp fmt "title"
      | Title_menu _ ->
          fp fmt "title_menu"
      | Pause_menu _ ->
          fp fmt "pause_menu"
      | Read_rules (p, _) ->
          fp fmt "read_rules (page %d)" p
      | Victory_screen _ ->
          fp fmt "victory"
      | Playing g ->
          fp fmt "playing (%a)" Game.Gameplay.pp_state g.gameplay
      | Waiting _ ->
          fp fmt "waiting"
      | End ->
          fp fmt "end" )
    t.kind

let has_error inputs =
  List.exists (function Input.Error _ -> true | _ -> false) inputs

let has_quit inputs =
  List.exists (function Input.Quit -> true | _ -> false) inputs

type next_fun =
  ?animations:Animation.t list -> ?speed:float -> ?themes:Themes.t -> kind -> t

(* Victory *)
let victory_reducer (next : next_fun) = next End

(* Title screen *)
let title_reducer (next : next_fun) themes =
  next (Title_menu (Menu.title_menu themes))

(* Menu *)
let title_menu_reducer (next : next_fun) menu themes inputs =
  if has_quit inputs then next End
  else
    let speed = ref 1. in
    let ns =
      List.fold_left
        (function
          | Title_menu menu -> (
              let cc = Menu.get_current_choice menu in
              function
              | Input.Validate when cc.final ->
                  if cc.header = "Play !" then (
                    let p1 =
                      Menu.get_choice_option menu "Red player"
                      |> Option.get |> Game.decode_ptype
                    in
                    let p2 =
                      Menu.get_choice_option menu "Blue player"
                      |> Option.get |> Game.decode_ptype
                    in
                    let points =
                      Menu.get_choice_option menu "Pawns"
                      |> Option.get |> int_of_string
                    in
                    speed :=
                      float
                        ( Menu.get_choice_option menu "Game speed"
                        |> Option.get |> int_of_string )
                      /. 100. ;
                    Playing (Game.default_game p1 p2 points) )
                  else if cc.header = "How to play" then Read_rules (0, menu)
                  else failwith "Invalid button"
              | Input.Previous_menu ->
                  Title_menu (Menu.move_highlighted menu (-1))
              | Input.Next_menu ->
                  Title_menu (Menu.move_highlighted menu 1)
              | Input.Previous_option ->
                  Title_menu (Menu.move_option menu (-1))
              | Input.Next_option ->
                  Title_menu (Menu.move_option menu 1)
              | _ ->
                  Title_menu menu )
          | stop ->
              fun _ -> stop )
        (Title_menu menu) inputs
    in
    let theme = Menu.get_choice_option menu "Theme" |> Option.get in
    let themes = Themes.{themes with selected= theme} in
    next ~speed:!speed ~themes ns

(* Menu *)
let pause_menu_reducer (next : next_fun) menu suspended themes inputs =
  if has_quit inputs then next suspended
  else
    let ns =
      List.fold_left
        (function
          | Pause_menu (menu, s) -> (
              let cc = Menu.get_current_choice menu in
              function
              | Input.Validate when cc.final ->
                  if cc.header = "Resume" then suspended
                  else if cc.header = "Main menu" then
                    Title_menu (Menu.title_menu themes)
                  else failwith "Invalid button"
              | Input.Previous_menu ->
                  Pause_menu (Menu.move_highlighted menu (-1), s)
              | Input.Next_menu ->
                  Pause_menu (Menu.move_highlighted menu 1, s)
              | Input.Previous_option ->
                  Pause_menu (Menu.move_option menu (-1), s)
              | Input.Next_option ->
                  Pause_menu (Menu.move_option menu 1, s)
              | _ ->
                  Pause_menu (menu, s) )
          | stop ->
              fun _ -> stop )
        (Pause_menu (menu, suspended))
        inputs
    in
    next ns

let read_rules_reducer (next : next_fun) page next_menu inputs =
  if has_quit inputs then next (Title_menu next_menu)
  else
    let n' =
      List.fold_left
        (fun n -> function
          | Input.Previous_option ->
              n - 1
          | Input.Next_option ->
              n + 1
          | _ ->
              n )
        page inputs
    in
    let next_page = Menu.modulo n' (Rules.nb_pages ()) in
    next (Read_rules (next_page, next_menu))

(* Playing *)
let playing_reducer (next : next_fun) game inputs =
  if has_quit inputs then next (Pause_menu (Menu.pause_menu (), Playing game))
  else
    match game.gameplay with
    | Game.Gameplay.Victory p ->
        next (Victory_screen p)
    | _ ->
        let game' = Game.next game inputs in
        next (Playing game')

let transition_trigger state new_state =
  let speed = state.speed in
  let anim_create = Animation.create ~speed in
  let wait_anim anim =
    let animations = anim :: new_state.animations in
    { kind= Waiting (anim.id, state.kind, new_state.kind)
    ; animations
    ; speed
    ; themes= new_state.themes }
  in
  let rec f state new_state done_list =
    let check_sound kind = not @@ List.exists (( = ) kind) done_list in
    let add_sound kind =
      let animations = anim_create 0.5 (Sound kind) :: new_state.animations in
      f state {new_state with animations} (kind :: done_list)
    in
    match (state.kind, new_state.kind) with
    (* Title screen *)
    | Title_screen, Title_menu _ ->
        wait_anim (anim_create title_time Title)
    (* Menu move *)
    | Title_menu {highlighted= h1; _}, Title_menu {highlighted= h2; _}
      when h1 <> h2 ->
        wait_anim (anim_create menu_move_time (Menu_move (h1, h2)))
    (* Menu move *)
    | Pause_menu ({highlighted= h1; _}, _), Pause_menu ({highlighted= h2; _}, _)
      when h1 <> h2 ->
        wait_anim (anim_create menu_move_time (Menu_move (h1, h2)))
    (* Menu option changed *)
    | Title_menu m1, Title_menu m2 when m1 <> m2 && check_sound `menu_option ->
        add_sound `menu_option
    (* Game rules page change *)
    | Read_rules (m1, _), Read_rules (m2, _)
      when m1 <> m2 && check_sound `menu_option ->
        add_sound `menu_option
    (* Menu validated *)
    | (Title_menu _ | Pause_menu _), (Playing _ | Read_rules _)
      when check_sound `select ->
        add_sound `select
    (* Turn begins *)
    | ( (Title_menu _ | Playing {gameplay= Play _ | Replay _; _})
      , Playing {gameplay= Begin_turn _; _} )
    | Playing {gameplay= Play _; _}, Playing {gameplay= Replay _; _}
      when check_sound `cup_full ->
        add_sound `cup_full
    (* Victory *)
    | Playing _, Victory_screen _ ->
        wait_anim (anim_create victory_time Victory)
    (* Pawn move *)
    | Playing {gameplay= Choose _; _}, Playing {gameplay= Play (_, choice); _}
      ->
        wait_anim (anim_create move_time (Pawn_moving choice))
    (* Score up *)
    | ( Playing {gameplay= Play (P1, _); logic= l1}
      , Playing {gameplay= Begin_turn _; logic= l2} )
      when l1.p1.points < l2.p1.points ->
        wait_anim (anim_create score_up_time (Score_up P1))
    | ( Playing {gameplay= Play (P2, _); logic= l1}
      , Playing {gameplay= Begin_turn _; logic= l2} )
      when l1.p2.points < l2.p2.points ->
        wait_anim (anim_create score_up_time (Score_up P2))
    (* No move *)
    | ( Playing {gameplay= Choose (_, d, _); _}
      , Playing {gameplay= Begin_turn _; _} ) ->
        wait_anim Animation.(anim_create cannot_choose_time (Cannot_choose d))
    (* Yellow choice *)
    | ( Playing {gameplay= Begin_turn _; _}
      , Playing ({gameplay= Choose (_p, _, _); _} as _g) )
    | ( Playing {gameplay= Replay _; _}
      , Playing ({gameplay= Choose (_p, _, _); _} as _g) ) ->
        wait_anim (anim_create choice_time Choice)
    | _ ->
        new_state
  in
  f state new_state []

let input_interrupts_wait_to next input =
  match (input, next) with
  | Input.Quit, Playing _
  | Input.Quit, Pause_menu _
  | Input.Quit, Title_menu _
  | Input.Previous_menu, Title_menu _
  | Input.Previous_menu, Pause_menu _
  | Input.Next_menu, Title_menu _
  | Input.Next_menu, Pause_menu _ ->
      true
  | _ ->
      false

(* Waiting *)
let rec waiting_reducer state aid _old next inputs =
  if List.exists (input_interrupts_wait_to next) inputs then
    (* Jump directly to interrupted reducer, carrying inputs, flushing waited animation *)
    let animations =
      List.filter Animation.(fun x -> x.id <> aid) state.animations
    in
    reducer {state with kind= next; animations} inputs
  else
    let b = List.exists Animation.(fun x -> x.id = aid) state.animations in
    if b then state else {state with kind= next}

(* Main switch *)
and reducer state inputs =
  let animations = List.filter Animation.is_active state.animations in
  let state = {state with animations} in
  if has_error inputs then {state with kind= End}
  else
    let next ?(animations = state.animations) ?(speed = state.speed)
        ?(themes = state.themes) kind =
      {kind; animations; speed; themes}
    in
    let new_state =
      match state.kind with
      | Title_screen ->
          title_reducer next state.themes
      | Title_menu m ->
          title_menu_reducer next m state.themes inputs
      | Read_rules (p, m) ->
          read_rules_reducer next p m inputs
      | Playing g ->
          playing_reducer next g inputs
      | Pause_menu (m, s) ->
          pause_menu_reducer next m s state.themes inputs
      | Victory_screen _ ->
          victory_reducer next
      | Waiting (aid, old, ns) ->
          waiting_reducer state aid old ns inputs
      | End ->
          state
    in
    if debug then Format.printf "%a -> %a@." pp state pp new_state ;
    transition_trigger state new_state
