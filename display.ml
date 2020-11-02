module type DISPLAY_ENGINE = sig
  val start : (unit -> State.t option) -> State.t -> unit
end

module Make (Engine : DISPLAY_ENGINE) = struct
  let channel = Event.new_channel ()

  let sync state =
    Event.send channel state |> Event.sync

  let thread = ref None

  let poll_state () = Event.receive channel |> Event.poll

  let init state =
    let start_thread () = Engine.start poll_state state in
    thread := Some (Thread.create start_thread ())

  let terminate () =
    match !thread with
    | None -> failwith "Display was never initialized!"
    | Some t -> Thread.join t
end
