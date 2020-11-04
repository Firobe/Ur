type kind =
  | Playing of Game.t
  | Waiting of int * kind
  | End

type t = {
  kind : kind;
  animations : Animation.t list;
  has_waited : bool
}

let has_quit inputs =
  List.exists ((=) Input.Quit) inputs

let reducer state inputs =
  let animations = List.filter Animation.is_active state.animations in
  let default kind = {kind; animations; has_waited = false} in
  if has_quit inputs then default End 
  else
    let rec f s = match s.kind with
    | Playing {gameplay = Victory _; _} -> default End
    | Playing {gameplay = Play (_, choice); _} when not s.has_waited ->
      let na = Animation.(create 0.7 (Pawn_moving choice)) in
      let animations = na :: animations in
      {kind = Waiting (na.id, s.kind); animations; has_waited = false}
    | Waiting (aid, _) when List.exists Animation.(fun x -> x.id = aid) animations ->
      {s with animations}
    | Waiting (_, next) -> f {kind = next; animations; has_waited = true}
    | Playing game ->
      if has_quit inputs then default End
      else 
        let game' = Game.next game in
        default (Playing game')
    | End -> {s with animations}
    in f state

