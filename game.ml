type playerNo = P1 | P2

let pp_player fmt = function
    | P1 -> Format.fprintf fmt "red"
    | P2 -> Format.fprintf fmt "blue"

module Logic = struct 
  type position =
    | Reserve
    | Intro of int (* [0-3] *)
    | Main of int  (* [0-7] *)
    | Outro of int (* [0-1] *)

  type pawn = {
    owner : playerNo;
    position : position;
  }

  type move =
    | Add
    | Move of position
    | Take of pawn
    | Finish

  type choice_function = (pawn * move) list -> int option

  type player = {
    reserve : int;
    points : int;
    choose : choice_function
  }

  type state = {
    p1 : player;
    p2 : player ;
    pawns : pawn list;
  }

  let max_pawns = 7

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

  let all_moves state player n =
    let own_pawns = List.filter (fun p -> p.owner = player) state.pawns in
    let existing = List.filter_map (fun p ->
        match compute_move state (p, n) with
        | None -> None
        | Some x -> Some (p, x)
      ) own_pawns in
    let p = if player = P1 then state.p1 else state.p2 in
    if p.reserve > 0 && n > 0 then
      match compute_move state ({owner = player; position = Reserve}, n) with
      | None -> existing
      | Some (Move x) -> ({owner = player; position = x}, Add) :: existing
      | _ -> failwith "Introducing pawn cannot take or finish"
    else existing

  let apply_move state pawn move =
    let remove_pawn pawns pawn = List.filter ((<>) pawn) pawns in
    let pawns = remove_pawn state.pawns pawn in
    let set_pos pawn position = {pawn with position} in
    let set_pawns state pawns = {state with pawns} in
    let replay p = if is_rose p then Some p else None in
    match move with
    | Finish ->
      let add_point p = {p with points = p.points + 1} in
      if pawn.owner = P1 then
        {state with pawns; p1 = add_point state.p1}, None 
      else
        {state with pawns; p2 = add_point state.p2}, None
    | Move pos ->
      set_pawns state ((set_pos pawn pos) :: pawns), replay pos
    | Take p' ->
      let add_reserve p = {p with reserve = p.reserve + 1} in
      let state = if pawn.owner = P2 then
          {state with pawns; p1 = add_reserve state.p1}
        else
          {state with pawns; p2 = add_reserve state.p2}
      in
      let pawns' = remove_pawn pawns p' in
      set_pawns state ((set_pos pawn p'.position) :: pawns'), replay p'.position
    | Add ->
      let remove_reserve p = {p with reserve = p.reserve - 1} in
      let state = if pawn.owner = P1 then
          {state with pawns; p1 = remove_reserve state.p1}
        else
          {state with pawns; p2 = remove_reserve state.p2}
      in set_pawns state (pawn :: pawns), replay pawn.position

  let check_end state =
    if state.p1.points = max_pawns then Some P1
    else if state.p2.points = max_pawns then Some P2
    else None

  let throw_dices () =
    let throw () =
      if Random.bool () then 1 else 0
    in
    throw () + throw () + throw () + throw ()
end

module AI = struct
  let common_ai sub am =
    if am = [] then begin None
    end else Some (sub am)

  let basic_ai =
    let pos_compare = compare in
    let open Logic in
    let sortfun (_, x) (_, y) = match (x, y) with
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
    let sub am =
      let v = am
              |> List.mapi (fun i x -> (i, x))
              |> List.sort sortfun |> List.rev |> List.hd in
      fst v
    in
    common_ai sub

  let random_ai : Logic.choice_function =
    let sub am = Random.int (List.length am) in
    common_ai sub
end

module Gameplay = struct
  type state =
    | Begin_turn of playerNo
    | Choose of playerNo * int * (Logic.pawn * Logic.move) list
    | Play of playerNo * (Logic.pawn * Logic.move)
    | Replay of playerNo * Logic.position
    | Victory of playerNo

  let pp_state fmt = function
    | Begin_turn p -> Format.fprintf fmt "begin %a" pp_player p
    | Choose (p, _, _) -> Format.fprintf fmt "wait %a" pp_player p
    | Play (p, _) -> Format.fprintf fmt "play %a" pp_player p
    | Replay (p, _) -> Format.fprintf fmt "replay %a" pp_player p
    | Victory p -> Format.fprintf fmt "victory %a" pp_player p

  let next_player player = if player = P1 then P2 else P1

  let begin_turn player logic =
    match Logic.check_end logic with
    | Some p -> (logic, Victory p)
    | None ->
      let n = Logic.throw_dices () in
      let am = Logic.all_moves logic player n in
      (logic, Choose (player, n, am))

  let wait_input player logic am =
    let open Logic in
    let p = if player = P1 then logic.p1 else logic.p2 in
    match p.choose am with
    | None -> (logic, Begin_turn (next_player player))
    | Some choice ->
      let pm = List.nth am choice in
      (logic, Play (player, pm))

  let play player logic (pawn, move) =
    let open Logic in
    let logic', replay = apply_move logic pawn move in
    begin match replay with
      | None -> (logic', Begin_turn (next_player player))
      | Some pos -> (logic', Replay (player, pos))
    end

  let replay player logic pos =
    let open Logic in
    let n = throw_dices () in
    let pawn =
      logic.pawns
      |> List.filter (fun p -> p.owner = player && p.position = pos)
      |> List.hd in
    begin match compute_move logic (pawn, n) with
      | Some move -> (logic, Choose (player, n, [(pawn, move)]))
      | None -> (logic, Begin_turn (next_player player))
    end

  let victory player logic = (logic, Victory player)
end

type t = {
  gameplay : Gameplay.state;
  logic : Logic.state;
}

let next game =
  let (logic, gameplay) = match game.gameplay with
  | Begin_turn p -> Gameplay.begin_turn p game.logic
  | Choose (p, _, am) -> Gameplay.wait_input p game.logic am
  | Play (p, pm) -> Gameplay.play p game.logic pm
  | Replay (p, pos) -> Gameplay.replay p game.logic pos
  | Victory p -> Gameplay.victory p game.logic
  in
  {logic; gameplay}
