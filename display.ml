module type DISPLAY_ENGINE = sig
  val start : (State.t -> State.t) -> State.t -> unit
end

module Make (Engine : DISPLAY_ENGINE) = struct
  let channel = Event.new_channel ()

  let sync state =
    Event.send channel state |> Event.sync

  let thread = ref None

  let update_state state =
    match Event.receive channel |> Event.poll with
    | None -> state
    | Some s -> s

  let init state =
    let start_thread () = Engine.start update_state state in
    thread := Some (Thread.create start_thread ())

  let terminate () =
    match !thread with
    | None -> failwith "Display was never initialized!"
    | Some t -> Thread.join t
end
