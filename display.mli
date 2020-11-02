module type DISPLAY_ENGINE = sig
  val start : poll_state:(unit -> State.t option)
           -> send_input:(Input.t -> unit)
           -> init_state:State.t
           -> unit
end

module Make : functor (_ : DISPLAY_ENGINE) -> sig
  val sync : State.t -> unit
  val poll_input : unit -> Input.t list
  val init : State.t -> unit
  val terminate : unit -> unit
end
