let rules =
  let normal =
    [ ( "(1/4) The game"
      , [ "This is a turn-based, where two players face each other."
        ; "Each player has a number of pawns in a reserve."
        ; "The goal of a player is to bring all pawns to his end of the board."
        ; "The first one to do so wins."
        ; "The tricky part is that the middle row is shared by the two players!"
        ] )
    ; ( "(2/4) Throwing the dice"
      , [ "At the beginning of his turn, the player must throw the dices"
        ; "by clicking on the cup or pressing [SPACE]."
        ; "There are four dice with four faces each and two of their corners"
        ; "are white. If a white corner is pointing upwards, then it counts as \
           a point."
        ; "The total number of points is how much the player can move this \
           turn."
        ; "If the total is zero, the player's turn is skipped." ] )
    ; ( "(3/4) Movement"
      , [ "You can either place a reserve pawn on the board if it lands on an \
           empty cell"
        ; "or move a present pawn if it lands on an empty cell or enemy pawn."
        ; "In this last case, the enemy pawn is placed back into his reserve."
        ; "A pawn can exit the board if it lands exactly on the exit, which \
           increases the score."
        ; "If a pawn lands on a special cell, then the player can immediately \
           throw"
        ; "the dice again to replay that pawn." ] )
    ; ( "(4/4) A bit of history"
      , [ "This game was first played in ancient Mesopotamia"
        ; "during the early third millenium BC, and was extremely"
        ; "popular across the world. It remained popular until late"
        ; "antiquity, and eventually completely forgotten." ] ) ]
  in
  List.map (fun (a, b) -> (a, List.rev b)) normal

let get_page n = List.nth rules n

let nb_pages () = List.length rules
