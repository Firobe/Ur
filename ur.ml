open State
module ADisplay = Display.Make (Gl_display)

let rec game_loop state =
  ADisplay.sync state ;
  if state.kind <> End then
    let inputs = ADisplay.wait_inputs () in
    let state' = State.reducer state inputs in
    game_loop state'

let data_dir = "data/"

let opam_share_dir () =
  try
    let inp = Unix.open_process_in "opam var ur:share" in
    let r = input_line inp in
    close_in inp ; r ^ "/"
  with _ -> "./"

let search_order = ["./"; "/usr/share/ur/"; opam_share_dir ()]

let test_dir path = Sys.file_exists path && Sys.is_directory path

let find_share_dir () =
  match
    List.fold_left
      (fun found path ->
        let data_path = path ^ data_dir in
        if Option.is_some found then found
        else if test_dir data_path then Some data_path
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

let _ =
  Printexc.record_backtrace true ;
  try go ()
  with e ->
    Printf.printf "FATAL: got exception %s\nBacktrace:\n%s\n%!"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
