open Common 

module Game (D : IO) = struct
(* ==== MOVEMENT ==== *)

let is_rose = function
  | Intro x -> x = 3
  | Main x -> x = 3
  | Outro _
  | Reserve -> false

(* pre : n <= 4 *)
let next_position n = function
  | Reserve ->                  `Inside (Intro (n - 1)) 
  | Intro x when x + n < 4 ->   `Inside (Intro (x + n))
  | Intro x ->                  `Inside (Main (x + n - 4))
  | Main x when x + n < 8 ->    `Inside (Main (x + n))
  | Main x when x + n - 8 < 2 ->`Inside (Outro (x + n - 8))
  | Main x when x + n - 8 = 2 ->`Finish
  | Main _ ->                   `Out_of_bounds
  | Outro x when x + n < 2 ->   `Inside (Outro (x + n))
  | Outro x when x + n = 2 ->   `Finish
  | Outro _ ->                  `Out_of_bounds

let already_on_position state player pos = 
  match pos with
  | Intro _
  | Outro _ -> List.find_opt (fun p -> p.owner = player && p.position = pos)
                 state.pawns
  | Main _ -> List.find_opt (fun p -> p.position = pos) state.pawns
  | Reserve -> None

let compute_move state (p, n) =
  match next_position n p.position with
  | `Out_of_bounds -> None 
  | `Finish ->        Some Finish
  | `Inside pos ->
    begin match already_on_position state p.owner pos with
      | None ->                             Some (Move pos)
      | Some p' when p'.owner = p.owner ->  None 
      | Some p' when not @@ is_rose pos ->  Some (Take p')
      | _ ->                                None 
    end

let all_moves state player n replay =
  let own_pawns = List.filter (fun p -> p.owner = player) state.pawns in
  let own_pawns = match replay with
    | None -> own_pawns
    | Some pos -> List.filter (fun p -> p.position = pos) own_pawns
  in
  let existing = List.filter_map (fun p ->
      match compute_move state (p, n) with
      | None -> None
      | Some x -> Some (p, x)
    ) own_pawns in
  let p = if player = P1 then state.p1 else state.p2 in
  if p.reserve > 0 && n > 0 && replay = None then
    match compute_move state ({owner = player; position = Reserve}, n) with
    | None -> existing
    | Some (Move x) -> ({owner = player; position = x}, Add) :: existing
    | _ -> failwith "Introducing pawn cannot take or finish"
  else existing

let apply_move state pawn move =
  let remove_pawn pawns pawn = List.filter ((<>) pawn) pawns in
  let pawns = remove_pawn state.pawns pawn in
  let draw' = D.draw_movement {state with pawns} in
  let set_pos pawn position = {pawn with position} in
  let set_pawns state pawns = {state with pawns} in
  let replay p = if is_rose p then Some p else None in
  match move with
    | Finish ->
      draw' pawn (Outro 2); (* Dirty hack ew *)
      let add_point p = {p with points = p.points + 1} in
      if pawn.owner = P1 then
        {state with pawns; p1 = add_point state.p1}, None 
      else
        {state with pawns; p2 = add_point state.p2}, None
    | Move pos ->
      draw' pawn pos;
      set_pawns state ((set_pos pawn pos) :: pawns), replay pos
    | Take p' ->
      draw' pawn p'.position;
      let add_reserve p = {p with reserve = p.reserve + 1} in
      let state = if pawn.owner = P2 then
        {state with pawns; p1 = add_reserve state.p1}
      else
        {state with pawns; p2 = add_reserve state.p2}
      in
      let pawns' = remove_pawn pawns p' in
      set_pawns state ((set_pos pawn p'.position) :: pawns'), replay p'.position
    | Add ->
      draw' {pawn with position = Reserve} pawn.position;
      let remove_reserve p = {p with reserve = p.reserve - 1} in
      let state = if pawn.owner = P1 then
        {state with pawns; p1 = remove_reserve state.p1}
      else
        {state with pawns; p2 = remove_reserve state.p2}
      in set_pawns state (pawn :: pawns), replay pawn.position


(* ======= GAMEPLAY ====== *)

let throw_dices () =
  let throw () =
    if Random.bool () then 1 else 0
  in
  throw () + throw () + throw () + throw ()

let check_end state =
  if state.p1.points = max_pawns then Some P1
  else if state.p2.points = max_pawns then Some P2
  else None

let rec play state player =
  match check_end state with
  | Some p -> D.draw_victory state p
  | None ->
    let p = if player = P1 then state.p1 else state.p2 in
    let next_player = if player = P1 then P2 else P1 in
    let rec play_once state replay =
      let n = throw_dices () in
      D.draw_state state;
      D.draw_dice player n;
      let am = all_moves state player n replay in
      match p.choose am with
      | None -> state
      | Some (pawn, move) ->
        let state', r = apply_move state pawn move in
        begin match r with
          | None -> state'
          | Some _ -> play_once state' r
        end
    in
    play (play_once state None) next_player

(* ==== ARTIFICIAL PLAYERS ==== *)

let common_ai sub am =
  if am = [] then begin
    D.draw_ia_cannot_move (); None
  end else Some (sub am)

let basic_ai =
    let pos_compare = compare in
    let sortfun x y = match (x, y) with
      | (_, Take {position=a; _}), (_, Take {position= b; _}) ->
        pos_compare a b
      | (_, Take _), _ -> 1
      | _, (_, Take _) -> -1
      | (_, Finish), (_, Finish) -> 0
      | (_, Finish), _ -> 1
      | _, (_, Finish) -> -1
      | ({position=a; _}, Add), ({position=b; _}, Add) ->
        pos_compare a b
      | (_, Add), _ -> 1
      | _, (_, Add) -> -1
      | (_, Move (Outro a)), (_, Move (Outro b)) -> compare a b
      | (_, Move (Outro _)), (_, Move _) -> 1
      | (_, Move _), (_, Move (Outro _)) -> -1
      | (_, Move (Intro a)), (_, Move (Intro b)) -> compare a b
      | (_, Move (Intro _)), (_, Move _) -> 1
      | (_, Move _), (_, Move (Intro _)) -> -1
      | (_, Move a), (_, Move b) -> pos_compare a b
    in
    let sub am = List.sort sortfun am |> List.rev |> List.hd in
    common_ai sub

let random_ai =
  let sub am = List.nth am @@ Random.int (List.length am) in
  common_ai sub

let go () =
  D.init ();
  Random.self_init ();
  let default_player = {
    reserve = max_pawns;
    points = 0;
    choose = D.ask_draw 
  } in
  let state = {
    p1 = {default_player with
          choose =
            if auto_mode then random_ai
            else D.ask_draw};
    p2 = {default_player with choose = basic_ai};
    pawns = []
  } in play state P1;
  D.terminate ()

end

let _ =
  let module FullGame = Game (Legacy) in
  FullGame.go ()
