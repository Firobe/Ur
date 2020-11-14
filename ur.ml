open State
module ADisplay = Display.Make (Opengl)

let rec game_loop state =
  ADisplay.sync state ;
  if state.kind <> End then
    let inputs = ADisplay.wait_inputs () in
    let state' = State.reducer state inputs in
    game_loop state'

let init_state = {kind= Title_screen; animations= []}

let go () =
  Random.self_init () ;
  ADisplay.init init_state ;
  game_loop init_state ;
  Format.printf "End of logic loop@." ;
  ADisplay.terminate () ;
  Format.printf "Terminated normally@."

let _ = go ()
