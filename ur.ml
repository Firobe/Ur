let max_pawns = 7
let ss = 40 (* Square size *)


type playerNo = P1 | P2

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

type player = {
  reserve : int;
  points : int;
  choose : (pawn * move) list -> (pawn * move)
}

type state = {
  p1 : player;
  p2 : player ;
  pawns : pawn list;
}

let is_rose = function
  | Intro x -> x = 3
  | Main x -> x = 3
  | Outro _
  | Reserve -> false

(* ==== DISPLAY ==== *)

open Graphics

let draw_board () =
  clear_graph ();
  set_color black;
  for x = 0 to 7 do
    for y = 0 to 2 do
      draw_rect (ss * x) (ss * y) ss ss
    done
  done;
  set_color white;
  fill_rect (4 * ss + 1) 0 (ss * 2 - 2) (ss - 1);
  fill_rect (4 * ss + 1) (ss * 2 + 1) (ss * 2 - 2) (ss - 1);
  set_color magenta;
  let draw_cross x y =
    moveto (x * ss + 2) (y * ss + 2);
    rlineto (ss - 4) (ss - 4);
    rmoveto (4 - ss) 0;
    rlineto (ss - 4) (4 - ss)
  in
  draw_cross 0 0;
  draw_cross 0 2;
  draw_cross 3 1

let pawn_to_coord = function
    | {owner=P1; position=Intro x} -> 3 - x, 0
    | {owner=P2; position=Intro x} -> 3 - x, 2
    | {owner=P1; position=Outro x} -> 6 + (1 - x), 0
    | {owner=P2; position=Outro x} -> 6 + (1 - x), 2
    | {position=Main x; _} -> x, 1
    | _ -> failwith "Cannot draw reserve pawn"

let draw_circle_grid x y r =
  fill_circle (ss * x + ss / 2) (ss * y + ss / 2) (ss / r)

let draw_pawn p =
  set_color (if p.owner = P1 then red else blue);
  let x, y = pawn_to_coord p in
  draw_circle_grid x y 3

let draw_state state =
  draw_board ();
  List.iter draw_pawn state.pawns;
  set_color red;
  moveto 5 (ss * 3 + 25);
  draw_string @@ Format.sprintf "Reserve %d  Points %d"
    state.p1.reserve state.p1.points;
  set_color blue;
  moveto 5 (ss * 3 + 10);
  draw_string @@ Format.sprintf "Reserve %d  Points %d"
    state.p2.reserve state.p2.points

let ask_draw am =
  List.iteri (fun i (p, _) ->
      set_color green;
      let x, y = pawn_to_coord p in
      draw_circle_grid x y 5;
      moveto (x * ss + 2) (y * ss + 2);
      set_color black;
      draw_string (string_of_int (i + 1))
    ) am;
  let rec choice_loop () =
    try
      let key = read_key () in
      let choice = int_of_string (String.make 1 key) in
      if choice <= List.length am && choice > 0 then choice
      else choice_loop ()
    with _ -> choice_loop ()
  in
  List.nth am (choice_loop () - 1)

let draw_info offset color s =
  set_color color;
  moveto 200 (ss * 3 + 17 - offset * 10);
  draw_string s

let init = 
  open_graph "";
  resize_window (ss * 9) (ss * 4)

let terminate () =
  close_graph ()

(* ==== MOVEMENT ==== *)

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


(* ======= GAMEPLAY ====== *)

let throw_dices () =
  let throw () =
    if Random.bool () then 1 else 0
  in
  throw () + throw () + throw () + throw ()

let pp_player fmt = function
    | P1 -> Format.pp_print_int fmt 1
    | P2 -> Format.pp_print_int fmt 2

let check_end state =
  if state.p1.points = max_pawns then Some P1
  else if state.p2.points = max_pawns then Some P2
  else None

let rec play state player =
  match check_end state with
  | Some p ->
    draw_state state;
    draw_info 0 magenta (Format.asprintf "Player %a wins!" pp_player p);
    ignore (read_key ())
  | None ->
    let p = if player = P1 then state.p1 else state.p2 in
    let next_player = if player = P1 then P2 else P1 in
    let rec play_once state replay =
      let n = throw_dices () in
      draw_state state;
      draw_info 0 (if player = P1 then red else blue)
        (Format.sprintf "You threw a %d!" n);
      let am = all_moves state player n replay in
      if am = [] then begin
        draw_info 1 magenta "No possible move ! (press any key)";
        ignore (read_key ());
        state
      end else begin
        let pawn, move = p.choose am in
        let state', r = apply_move state pawn move in
        match r with
        | None -> state'
        | Some _ -> play_once state' r
      end
    in
    play (play_once state None) next_player

let basic_ai am =
  let pos_compare = compare in
  let sortfun x y = match (x, y) with
    | (_, Finish), (_, Finish) -> 0
    | (_, Finish), _ -> 1
    | _, (_, Finish) -> -1
    | (_, Take {position=a; _}), (_, Take {position= b; _}) ->
      pos_compare a b
    | (_, Take _), _ -> 1
    | _, (_, Take _) -> -1
    | ({position=a; _}, Add), ({position=b; _}, Add) ->
      pos_compare a b
    | (_, Add), _ -> 1
    | _, (_, Add) -> -1
    | (_, Move a), (_, Move b) -> pos_compare a b
  in
  let choice = List.sort sortfun am |> List.rev |> List.hd in
  set_color green;
  let x, y = pawn_to_coord (fst choice) in
  draw_circle_grid x y 5;
  draw_info 1 magenta "AI chooses to move this pawn";
  Unix.sleepf 2.; choice

let auto_choose am = Unix.sleepf 0.5; List.hd am

let main =
  Random.self_init ();
  let default_player = {
    reserve = max_pawns;
    points = 0;
    choose = ask_draw 
  } in
  let state = {
    p1 = {default_player with choose = ask_draw};
    p2 = {default_player with choose = basic_ai};
    pawns = []
  } in play state P1;
  terminate ()

