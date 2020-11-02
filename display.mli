module type DISPLAY_ENGINE = sig
  val start : (unit -> State.t option) -> State.t -> unit
end

module Make : functor (_ : DISPLAY_ENGINE) -> sig
  val channel : State.t Event.channel
  val sync : State.t -> unit
  val init : State.t -> unit
  val terminate : unit -> unit
end
