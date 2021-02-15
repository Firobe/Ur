(
 (font "%SHARED%/klill.ttf")
 (text_styles (
               (base ((color (0 0 0))))
               (menu_selected ((color (255 165 0))))
               (alert ((color (255 165 0))))
               (p1 ((color (128 0 0))))
               (p2 ((color (0 0 128))))
               ))
 (background (texture ((name "bg_day.png") (w 11.) (h 5.))))
 (board (texture ((name "bgA.png") (w 11.) (h 5.))))
 (dice_style (animated (
                        (empty_cup ((name "%SHARED%/mc_tex/emptycup.png") (y -0.07) (w 2.0) (h 2.0)))
                        (fallen_cup ((name "%SHARED%/mc_tex/throwingcup.png") (y -0.07) (w 2.0) (h 2.0)))
                        (full_cup ((name "%SHARED%/mc_tex/filledcup.png") (y -0.07) (w 2.) (h 2.)))
                        (dice_1 (
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ))
                        (dice_2 (
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualone.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ((name "%SHARED%/mc_tex/DiceEqualzero.png") (x -0.25) (w 0.5) (h 0.5))
                                 ))
                        )))
 (rule_arrows ((name "%SHARED%/arrows.png") (w 11.) (h 5.)))
 (p1_pawn (texture ((name "%SHARED%/mc_tex/Red Pawn.png"))))
 (p2_pawn (texture ((name "%SHARED%/mc_tex/Blue Pawn.png"))))
 (p1_pawn_alt (texture ((name "%SHARED%/mc_tex/Red Pawn(Yellow).png"))))
 (p2_pawn_alt (texture ((name "%SHARED%/mc_tex/Blue Pawn(Yellow).png"))))
 (hollow_pawn (texture ((name "%SHARED%/mc_tex/yellow pawn.png") (x 0.5) (y -0.5))))
 (sounds (
          (title "%SHARED%/mc_sounds/title.ogx" 0.2)
          (victory "%SHARED%/mc_sounds/win.ogx" 0.2)
          (menu_choice "%SHARED%/mc_sounds/updown.ogx" 1.0)
          (menu_option "%SHARED%/mc_sounds/leftright.ogx" 0.5)
          (select "%SHARED%/mc_sounds/select.ogx" 1.0)
          (eaten "%SHARED%/mc_sounds/death.ogx" 0.4)
          (spawn "%SHARED%/mc_sounds/spawn.ogx" 0.5)
          (moving "%SHARED%/mc_sounds/moving.ogx" 0.5)
          (cup_full "%SHARED%/mc_sounds/cupfull.ogx" 1.0)
          (cup_thrown "%SHARED%/mc_sounds/throw.ogx" 1.0)
          (no_move "%SHARED%/mc_sounds/nomove.ogx" 0.5)
          ))
 )
