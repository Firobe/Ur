open Game
open Game.Logic

let ss = 40.
let pawn_radius = ss /. 3.

let iof = int_of_float
let foi = float_of_int

type context = {
  window : SFRenderWindow.t;
  font : SFFont.t
}

let pawn_to_coord = function
    | {owner=P1; position=Intro x} -> 3 - x, 0
    | {owner=P2; position=Intro x} -> 3 - x, 2
    | {owner=P1; position=Outro x} -> 6 + (1 - x), 0
    | {owner=P2; position=Outro x} -> 6 + (1 - x), 2
    | {position=Main x; _} -> x, 1
    | {owner=P1; position=Reserve} -> 4, 0
    | {owner=P2; position=Reserve} -> 4, 2

let red_circle =
  let circle = SFCircleShape.create ~radius:pawn_radius ~pointCount:32 () in
  SFCircleShape.setFillColor circle SFColor.red; circle

let blue_circle =
  let circle = SFCircleShape.create ~radius:pawn_radius ~pointCount:32 () in
  SFCircleShape.setFillColor circle SFColor.blue; circle

let draw_pawn window pawn =
  let x, y = pawn_to_coord pawn in
  let fx, fy = float_of_int x, float_of_int y in
  let circle = if pawn.owner = P1 then red_circle else blue_circle in
  SFCircleShape.setPosition ~circle ~position:(fx *. ss, fy *. ss);
  SFRenderWindow.drawCircleShape window ~circle ()

let draw_playing game context =
  let open SFRenderWindow in
  clear context.window SFColor.black;
  List.iter (draw_pawn context.window) game.logic.pawns;
  display context.window

let rec loop update_state state context =
  let state = update_state state in
  match state with
  | State.Playing game ->
    draw_playing game context;
    loop update_state state context
  | State.End ->
    Format.printf "End of display loop@."; context

let init () =
  let iss = iof ss in
  let window = SFRenderWindow.make (9 * iss, 7 * iss) "Royal Game of Ur" in
  let font = SFFont.createFromFile ~filename:"./Compagnon-Light.otf" in
  {window; font}

let terminate context =
  SFRenderWindow.destroy context.window

let start update_state state =
  init () |> loop update_state state |> terminate
