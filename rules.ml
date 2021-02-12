let rules =
  let open Game.Logic in
  let normal =
    [ ( "(1/7) The game"
      , [`Board; `Reserve (7, 7)]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "This is a two-player, turn-based game. Each player starts with \
           seven pawns in reserve." ] )
    ; ( "(2/7) Goal and movement"
      , [`Board; `Arrows]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "The players must move all their pawns from reserve to exit along \
           different paths with a common part." ] )
    ; ( "(2/7) Throwing the dice"
      , [`Cup]
      , [ "At the beginning of his turn, the player must throw the four dices"
        ; "by clicking on the bottom-left cup or pressing [SPACE]."
        ; " "
        ; "The number of white corners facing upwards"
        ; "is how much the player can move this turn."
        ; "If the total is zero, the player's turn is skipped." ] )
    ; ( "(3/7) Introducing pawns"
      , [`Board; `Empty_pawn {owner= P1; position= Intro 2}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "After the dice throw, a player can choose to move a pawn from the \
           reserve to the board if there's room." ] )
    ; ( "(3/7) Introducing pawns"
      , [`Board; `Pawn {owner= P1; position= Intro 2}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "After the dice throw, a player can choose to move a pawn from the \
           reserve to the board if there's room." ] )
    ; ( "(4/7) Moving pawns"
      , [`Board; `Full_pawn {owner= P1; position= Intro 2}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "One can also move any existing pawn if the pawn lands on an empty \
           cell or an enemy pawn." ] )
    ; ( "(4/7) Moving pawns"
      , [`Board; `Pawn {owner= P1; position= Main 0}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "One can also move any existing pawn if the pawn lands on an empty \
           cell or an enemy pawn." ] )
    ; ( "(5/7) Attacking your opponent"
      , [ `Board
        ; `Full_pawn {owner= P1; position= Main 1}
        ; `Pawn {owner= P2; position= Main 4}
        ; `Reserve (6, 6) ]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "If you land on an enemy, his pawn returns back to the reserve." ] )
    ; ( "(5/7) Attacking your opponent"
      , [`Board; `Pawn {owner= P1; position= Main 4}; `Reserve (6, 7)]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "If you land on an enemy, his pawn returns back to the reserve." ] )
    ; ( "(6/7) Replaying"
      , [`Board; `Full_pawn {owner= P1; position= Main 1}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "If you land on a special cell with a rose symbol, you can throw the \
           dice and replay that pawn." ] )
    ; ( "(6/7) Replaying"
      , [`Board; `Full_pawn {owner= P1; position= Main 3}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "If you land on a special cell with a rose symbol, you can throw the \
           dice and replay that pawn." ] )
    ; ( "(6/7) Replaying"
      , [`Board; `Pawn {owner= P1; position= Main 6}]
      , [ " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; " "
        ; "If you land on a special cell with a rose symbol, you can throw the \
           dice and replay that pawn." ] )
    ; ( "(7/7) A bit of history"
      , []
      , [ "This game was first played in ancient Mesopotamia"
        ; "during the early third millenium BC, and was extremely"
        ; "popular across the world. It remained popular until late"
        ; "antiquity, and eventually completely forgotten." ] ) ]
  in
  List.map (fun (a, b, c) -> (a, b, List.rev c)) normal

let get_page n = List.nth rules n

let nb_pages () = List.length rules
