open Game
open State

let rec game_loop state =
  Display.sync state;
  if state <> End then begin
    (* get inputs *)
    let state' = State.reducer state in
    game_loop state'
  end

let init_state =
  let default_player = Logic.{
    reserve = max_pawns;
    points = 0;
    choose = AI.random_ai
  } in
  let logic = Logic.{
      p1 = default_player;
      p2 = {default_player with choose = AI.basic_ai};
      pawns = []
    } in
  let gameplay = Gameplay.Begin_turn P1 in
  let game = {logic; gameplay} in
  Playing game


let go () =
  Random.self_init ();
  Display.init init_state;
  game_loop init_state;
  Display.terminate ()

let _ = go ()
