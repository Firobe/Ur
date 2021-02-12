open State
module ADisplay = Display.Make (Gl_display)

let rec game_loop state =
  ADisplay.sync state ;
  if state.kind <> End then
    let inputs = ADisplay.wait_inputs () in
    let state' = State.reducer state inputs in
    game_loop state'

(* TODO also search opam share *)
let search_order = ["./data/"; "/usr/share/ur/data/"]

let test_dir path = Sys.file_exists path && Sys.is_directory path

let find_share_dir () =
  match
    List.fold_left
      (fun found path ->
        if Option.is_some found then found
        else if test_dir path then Some path
        else None )
      None search_order
  with
  | Some path ->
      path
  | None ->
      failwith "Could not find data folder !"

let init_state =
  let share = find_share_dir () in
  Format.printf "Using data dir: %s@." share ;
  { kind= Title_screen
  ; animations= []
  ; speed= 1.0
  ; themes= Themes.load_themes share }

let go () =
  Random.self_init () ;
  ADisplay.init init_state ;
  game_loop init_state ;
  Format.printf "End of logic loop@." ;
  ADisplay.terminate () ;
  Format.printf "Terminated normally@."

let _ = go ()
