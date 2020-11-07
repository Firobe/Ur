open Game
open Game.Gameplay

let move_time = 0.3
let title_time = 1.0
let victory_time = 2.0
let choice_time = 0.1

type kind =
  | Title_screen
  | Playing of Game.t
  | Waiting of int * kind
  | Victory_screen of playerNo
  | End

type t = {
  kind : kind;
  animations : Animation.t list;
  has_waited : bool
}

let is_same_kind k1 k2 = match k1, k2 with
  | Playing g1, Playing g2 ->
    Game.Gameplay.is_similar_state g1.gameplay g2.gameplay
  | Waiting _, Waiting _ -> true
  | Title_screen, Title_screen -> true
  | Victory_screen _, Victory_screen _ -> true
  | End, End -> true
  | _ -> false

let pp fmt t =
  Format.fprintf fmt "{has_waited=%B; animations[%d]; kind=%s"
    t.has_waited (List.length t.animations)
    (match t.kind with
     | Title_screen -> "title"
     | Victory_screen _ -> "victory"
     | Playing _ -> "playing"
     | Waiting _ -> "waiting"
     | End -> "end"
    )

let has_quit inputs =
  List.exists ((=) Input.Quit) inputs

type next_fun = ?animations:(Animation.t list) -> kind -> t

let anim_once (next : next_fun) after anim state =
  if state.has_waited then
    let after' = next after in
    if is_same_kind after'.kind state.kind then {after' with has_waited = true}
    else after'
  else
    let a = anim () in
    let animations = a :: state.animations in
    next ~animations (Waiting (a.id, state.kind))

(* Victory *)
let victory_reducer (next : next_fun) =
  anim_once next End (fun () -> Animation.(create victory_time Victory))

(* Title screen *)
let title_reducer (next : next_fun) =
  anim_once next (Playing (Game.default_game ()))
    (fun () -> Animation.(create title_time Title))

(* Playing *)
let playing_reducer (next : next_fun) state game inputs =
  match game.gameplay with
  | Game.Gameplay.Victory p -> next (Victory_screen p)
  | Play (_, choice) ->
    let after = Game.next game inputs in
    anim_once next (Playing after)
      (fun () -> Animation.(create move_time (Pawn_moving choice))) state
  | Choose (p, _, _) ->
    let after = Game.next game inputs in
    begin match (if p = P1 then game.logic.p1 else game.logic.p2).p_type with
      | Game.Logic.Human_player ->
        anim_once next (Playing after)
          (fun () -> Animation.(create choice_time Choice)) state
      | _ -> next (Playing after)
    end
  | _ -> 
    let game' = Game.next game inputs in
    next (Playing game')

(* Main switch *)
let reducer state inputs =
  let animations = List.filter Animation.is_active state.animations in
  let state = {state with animations} in
  if has_quit inputs then {state with kind = End; has_waited = false}
  else
    let rec f s =
      let next ?(animations=s.animations) kind =
        {kind; animations; has_waited = false}
      in
      match s.kind with
      | Title_screen -> title_reducer next s
      | Playing g -> playing_reducer next s g inputs
      | Victory_screen _ -> victory_reducer next s
      | Waiting (aid, _) when List.exists Animation.(fun x -> x.id = aid) animations -> s
      | Waiting (_, next) -> f {s with kind = next; has_waited = true}
      | End -> s
    in f state

