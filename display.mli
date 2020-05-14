module type DISPLAY_ENGINE = sig
  val start : (State.t -> State.t) -> State.t -> unit
end

module Make : functor (_ : DISPLAY_ENGINE) -> sig
  val channel : State.t Event.channel
  val sync : State.t -> unit
  val init : State.t -> unit
  val terminate : unit -> unit
end
