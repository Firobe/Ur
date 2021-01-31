open Game
open Game.Gameplay

let move_time = 0.3
let title_time = 1.0
let victory_time = 2.0
let choice_time = 0.1
let debug = false 

type kind =
  | Title_screen
  | Playing of Game.t
  | Waiting of int * kind * kind
  | Victory_screen of playerNo
  | End

type t = {kind: kind; animations: Animation.t list}

let is_same_kind k1 k2 =
  match (k1, k2) with
  | Playing g1, Playing g2 ->
      Game.Gameplay.is_similar_state g1.gameplay g2.gameplay
  | Waiting _, Waiting _ -> true
  | Title_screen, Title_screen -> true
  | Victory_screen _, Victory_screen _ -> true
  | End, End -> true
  | _ -> false

let pp fmt t =
  let fp = Format.fprintf in
  fp fmt "{animations[%d]; kind=%a}" (List.length t.animations)
    (fun fmt -> function Title_screen -> fp fmt "title"
      | Victory_screen _ -> fp fmt "victory"
      | Playing g -> fp fmt "playing (%a)" Game.Gameplay.pp_state g.gameplay
      | Waiting _ -> fp fmt "waiting" | End -> fp fmt "end")
    t.kind

let has_quit inputs =
  List.exists (function
      | Input.Quit
      | Input.Error _ -> true
      | _ -> false
    ) inputs

type next_fun = ?animations:Animation.t list -> kind -> t

(* Victory *)
let victory_reducer (next : next_fun) = next End

(* Title screen *)
let title_reducer (next : next_fun) = next (Playing (Game.default_game ()))

(* Playing *)
let playing_reducer (next : next_fun) game inputs =
  match game.gameplay with
  | Game.Gameplay.Victory p -> next (Victory_screen p)
  | _ ->
      let game' = Game.next game inputs in
      next (Playing game')

let transition_trigger state new_state =
  let wait_anim anim =
    let animations = anim :: new_state.animations in
    {kind= Waiting (anim.id, state.kind, new_state.kind); animations} in
  match (state.kind, new_state.kind) with
  | Title_screen, Playing _ -> wait_anim Animation.(create title_time Title)
  | Playing _, Victory_screen _ ->
      wait_anim Animation.(create victory_time Victory)
  | Playing {gameplay= Choose _; _}, Playing {gameplay= Play (_, choice); _} ->
      wait_anim Animation.(create move_time (Pawn_moving choice))
  | ( Playing {gameplay= Begin_turn _; _}
    , Playing ({gameplay= Choose (p, _, _); _} as g) )
   |( Playing {gameplay= Replay _; _}
    , Playing ({gameplay= Choose (p, _, _); _} as g) ) -> (
      let player = if p = P1 then g.logic.p1 else g.logic.p2 in
      match player.p_type with
      | Human_player -> wait_anim Animation.(create choice_time Choice)
      | _ -> new_state )
  | _ -> new_state

(* Waiting *)
let waiting_reducer state aid _old next =
  let b = List.exists Animation.(fun x -> x.id = aid) state.animations in
  if b then state else {state with kind= next}

(* Main switch *)
let reducer state inputs =
  let animations = List.filter Animation.is_active state.animations in
  let state = {state with animations} in
  if has_quit inputs then {state with kind= End}
  else
    let next ?(animations = state.animations) kind = {kind; animations} in
    let new_state =
      match state.kind with
      | Title_screen -> title_reducer next
      | Playing g -> playing_reducer next g inputs
      | Victory_screen _ -> victory_reducer next
      | Waiting (aid, old, next) -> waiting_reducer state aid old next
      | End -> state in
    if debug then Format.printf "%a -> %a@." pp state pp new_state ;
    transition_trigger state new_state
