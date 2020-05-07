open Common
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
    | {owner=P1; position=Reserve} -> 4, 0
    | {owner=P2; position=Reserve} -> 4, 2

let draw_circle_grid x y r =
  fill_circle (ss * x + ss / 2) (ss * y + ss / 2) (ss / r)

let draw_pawn p =
  set_color (if p.owner = P1 then red else blue);
  let x, y = pawn_to_coord p in
  draw_circle_grid x y 3

let draw_state state =
  auto_synchronize false;
  draw_board ();
  List.iter draw_pawn state.pawns;
  set_color red;
  moveto 5 (ss * 3 + 25);
  draw_string @@ Format.sprintf "Reserve %d  Points %d"
    state.p1.reserve state.p1.points;
  set_color blue;
  moveto 5 (ss * 3 + 10);
  draw_string @@ Format.sprintf "Reserve %d  Points %d"
    state.p2.reserve state.p2.points;
  auto_synchronize true

let draw_info offset color s =
  set_color color;
  moveto 200 (ss * 3 + 17 - offset * 10);
  draw_string s

let draw_movement state pawn new_pos =
  draw_state state;
  let image = get_image 0 0 (ss * 9) (ss * 4) in
  let foi = Float.of_int in
  let iof = Int.of_float in
  let x1, y1 = pawn_to_coord pawn in
  let x2, y2 = pawn_to_coord {pawn with position = new_pos} in
  let delta_x = x2 - x1 in
  let delta_y = y2 - y1 in
  let precision = 100 in
  let fx1, fy1, fdx, fdy = foi x1, foi y1, foi delta_x, foi delta_y in
  set_color (if pawn.owner = P1 then red else blue);
  for t = 0 to precision do
    let ft = (foi t) /. (foi precision) *. Float.pi in
    let raw = Float.sin (ft -. Float.pi /. 2.) in
    let normalized = (raw +. 1.) /. 2. in
    let xt = iof ((fx1 +. normalized *. fdx) *. (foi ss)) in
    let yt = iof ((fy1 +. normalized *. fdy) *. (foi ss)) in
    clear_graph ();
    draw_image image 0 0;
    fill_circle (xt + ss / 2) (yt + ss / 2) (ss / 3);
    Unix.sleepf (0.005 *. ia_wait)
  done

let pp_player fmt = function
    | P1 -> Format.fprintf fmt "red"
    | P2 -> Format.fprintf fmt "blue"

let draw_victory state p =
  draw_state state;
  draw_info 0 magenta (Format.asprintf "Player %a wins!" pp_player p);
  draw_info 1 black "Press any key";
  ignore (read_key ())

let draw_dice player n =
  draw_info 0 (if player = P1 then red else blue)
    (Format.sprintf "You threw a %d!" n)

let draw_ia_cannot_move () =
  draw_info 1 magenta "IA cannot move !";
  Unix.sleepf ia_wait

let init () = 
  open_graph "";
  resize_window (ss * 9) (ss * 4)

let terminate () =
  close_graph ()

let ask_draw am =
  if am = [] then begin
    draw_info 1 magenta "You cannot move ! (press any key)";
    ignore (read_key ());
    None
  end else begin
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
    Some (List.nth am (choice_loop () - 1))
  end

