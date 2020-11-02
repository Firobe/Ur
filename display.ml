module type DISPLAY_ENGINE = sig
  val start : poll_state:(unit -> State.t option)
           -> send_input:(Input.t -> unit)
           -> init_state:State.t
           -> unit
end

module Make (Engine : DISPLAY_ENGINE) = struct
  let thread = ref None

  let state_channel = Event.new_channel ()
  let input_channel = Event.new_channel ()
  let input_buffer = ref []

  let sync state =
    Event.send state_channel state |> Event.sync

  let send_input input =
    let acc = input :: !input_buffer in
    match Event.send input_channel acc |> Event.poll with
    | Some () -> input_buffer := []
    | None -> input_buffer := acc

  let poll_state () = Event.receive state_channel |> Event.poll
  let poll_input () =
    match Event.receive input_channel |> Event.poll with
    | Some l -> l
    | None -> []

  let init init_state =
    let start_thread () = Engine.start ~poll_state ~send_input ~init_state in
    thread := Some (Thread.create start_thread ())

  let terminate () =
    match !thread with
    | None -> failwith "Display was never initialized!"
    | Some t -> Thread.join t
end
