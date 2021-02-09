(
 (font "klill.ttf")
 (text_colors (
               (base (0 0 0))
               (menu_selected (255 165 0))
               (alert (255 165 0))
               (p1 (128 0 0))
               (p2 (0 0 128))
               ))
 (background (texture ((name "Background.png") (w 11.) (h 5.))))
 (board (texture ((name "Ur.png") (w 11.) (h 5.))))
 (dice_style (animated (
                        (empty_cup ((name "CupA.png") (y -0.07) (w 2.0) (h 2.0)))
                        (fallen_cup ((name "CupB.png") (y -0.07) (w 2.0) (h 2.0)))
                        (full_cup ((name "CupC.png") (y -0.07) (w 2.) (h 2.)))
                        (dice_1 (
                                 ((name "DICEA/DiceA3.png") (x -0.5))
                                 ((name "DICEA/DiceA5.png") (x -0.5))
                                 ((name "DICEA/DiceA6.png") (x -0.5))
                                 ((name "DICEA/DiceA1.png") (x -0.5))
                                 ((name "DICEA/DiceA2.png") (x -0.5))
                                 ((name "DICEA/DiceA4.png") (x -0.5))
                                 ))
                        (dice_2 (
                                 ((name "DICEB/DiceB3.png") (x -0.5))
                                 ((name "DICEB/DiceB4.png") (x -0.5))
                                 ((name "DICEB/DiceB5.png") (x -0.5))
                                 ((name "DICEB/DiceB1.png") (x -0.5))
                                 ((name "DICEB/DiceB2.png") (x -0.5))
                                 ((name "DICEB/DiceB6.png") (x -0.5))
                                 ))
                        )))
 (p1_pawn (texture ((name "Red_Pawn.png"))))
 (p2_pawn (texture ((name "Blue_Pawn.png"))))
 (p1_pawn_alt (texture ((name "Red_PawnYellow.png"))))
 (p2_pawn_alt (texture ((name "Blue_PawnYellow.png"))))
 (hollow_pawn (texture ((name "Yellow_Pawn.png") (x 0.5) (y -0.5))))
 (sounds (
          (title "sounds/launch&orWinA.ogg" 0.2)
          (menu_choice "sounds/updown.ogg" 1.0)
          (menu_option "sounds/LeftRight.ogg" 0.5)
          (select "sounds/ButtonPress&Select.ogg" 1.0)
          (spawn "sounds/S-Pawned.ogg" 0.2)
          (eaten "sounds/Death.ogg" 1.0)
          (moving "sounds/PawnMoving.ogg" 0.2)
          (cup_full "sounds/DiceGoCup.ogg" 1.0)
          (cup_thrown "sounds/DiceThrown.ogg" 1.0)
          (no_move "sounds/NoMove.ogg" 0.5)
          ))
 )
