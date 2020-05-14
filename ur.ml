open Game
open State

module ADisplay = Display.Make (Sfml)

let rec game_loop state =
  ADisplay.sync state;
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
  ADisplay.init init_state;
  game_loop init_state;
  Format.printf "End of logic loop@.";
  ADisplay.terminate ();
  Format.printf "Terminated normally@."

let _ = go ()
