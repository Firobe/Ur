(
 (font "klill.ttf")
 (text_colors (
   (base (0 0 0))
   (menu_selected (255 165 0))
   (alert (200 0 0))
   (p1 (128 0 0))
   (p2 (0 0 128))
 ))
 (background (texture ((name "Background.png") (w 11.) (h 5.))))
 (board (texture ((name "Ur.png") (w 11.) (h 5.))))
 (dice_style (animated (
   (empty_cup ((name "CupA.png") (y -0.07) (w 2.0) (h 2.0)))
   (fallen_cup ((name "CupB.png") (y -0.07) (w 2.0) (h 2.0)))
   (full_cup ((name "CupD.png") (y -0.07) (w 2.) (h 2.)))
 )))
 (p1_pawn (texture ((name "Red_Pawn.png"))))
 (p2_pawn (texture ((name "Blue_Pawn.png"))))
 (p1_pawn_alt (texture ((name "Red_PawnYellow.png"))))
 (p2_pawn_alt (texture ((name "Blue_PawnYellow.png"))))
 (hollow_pawn (texture ((name "Yellow_Pawn.png") (x 0.5) (y -0.5))))
)
