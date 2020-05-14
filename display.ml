let channel = Event.new_channel ()

let sync state =
  Event.send channel state |> Event.sync

let thread = ref None

let rec display_loop state =
  let state = match Event.receive channel |> Event.poll with
    | None -> state
    | Some s -> s
  in

  match state with 
  | State.Playing game ->
    Format.printf "Display : %a@." Game.Gameplay.pp_state game.gameplay;
    display_loop state
  | State.End -> Printf.printf "End of display loop\n"

let init state =
  thread := Some (Thread.create display_loop state)

let terminate () =
  match !thread with
  | None -> failwith "Display was never initialized!"
  | Some t -> Thread.join t

