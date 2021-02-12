module type DISPLAY_ENGINE = sig
  val start :
       poll_state:(unit -> State.t option)
    -> buffer_input:(Input.t -> unit)
    -> send_inputs:(unit -> unit)
    -> init_state:State.t
    -> error:(at_init:bool -> string -> unit)
    -> unit
end

module Make (Engine : DISPLAY_ENGINE) = struct
  let thread = ref None

  let state_channel = Event.new_channel ()

  let input_channel = Event.new_channel ()

  let input_buffer = ref []

  let sync state = Event.send state_channel state |> Event.sync

  let buffer_input input = input_buffer := input :: !input_buffer

  let send_inputs () =
    match Event.send input_channel !input_buffer |> Event.poll with
    | Some () ->
        input_buffer := []
    | None ->
        ()

  let error ~at_init msg =
    if at_init then Event.receive state_channel |> Event.sync |> ignore ;
    Event.send input_channel [Input.Error msg] |> Event.sync ;
    Event.receive state_channel |> Event.sync |> ignore

  (* Ignore last state *)

  let poll_state () = Event.receive state_channel |> Event.poll

  let wait_inputs () = Event.receive input_channel |> Event.sync

  let init init_state =
    let start_thread () =
      Printf.printf "Display engine started.\n%!" ;
      Engine.start ~poll_state ~buffer_input ~send_inputs ~init_state ~error ;
      Printf.printf "Display engine stopped.\n%!"
    in
    thread := Some (Thread.create start_thread ())

  let terminate () =
    match !thread with
    | None ->
        failwith "Display was never initialized!"
    | Some t ->
        Thread.join t
end
