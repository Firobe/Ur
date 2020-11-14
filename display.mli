module type DISPLAY_ENGINE = sig
  val start :
       poll_state:(unit -> State.t option)
    -> buffer_input:(Input.t -> unit)
    -> send_inputs:(unit -> unit)
    -> init_state:State.t
    -> unit
end

module Make : functor (_ : DISPLAY_ENGINE) -> sig
  val sync : State.t -> unit
  val wait_inputs : unit -> Input.t list
  val init : State.t -> unit
  val terminate : unit -> unit
end
