open Tgl4
open Gl_utils

let pi = 3.14159265359

let trig_norm x = 2. *. pi *. x

let trig_counter i n = trig_norm @@ (float i /. float n)

let smooth_progress prog = (cos (pi +. (prog *. pi)) +. 1.) /. 2.

let triangle r g b =
  ( Gl.triangles
  , [|-1.0; -1.0; 0.0; 0.0; 1.0; 0.0; 1.0; -1.0; 0.0|]
  , [|r; g; b; r; g; b; r; g; b|]
  , [|0; 1; 2|] )

let set_3d a i x y z =
  let start = i * 3 in
  a.(start) <- x ;
  a.(start + 1) <- y ;
  a.(start + 2) <- z

module Counter = struct
  type kind = Raw | Trig | Cos | Sin

  type t = {mutable n: int; max: int; kind: kind}

  let create kind max = {n= 0; max; kind}

  let incr t = t.n <- (t.n + 1) mod t.max

  let get t =
    match t.kind with
    | Raw ->
        float t.n
    | Trig ->
        trig_counter t.n t.max
    | Cos ->
        cos (trig_counter t.n t.max)
    | Sin ->
        sin (trig_counter t.n t.max)
end

let circle r g b n ra =
  let vertices = Array.make ((n + 2) * 3) 0. in
  let colors = Array.make ((n + 2) * 3) 0. in
  for i = 1 to n + 1 do
    let step = trig_counter (i - 1) n in
    set_3d vertices i (cos step *. ra) (sin step *. ra) 0. ;
    set_3d colors i r g b
  done ;
  let indices = Array.init (n + 2) (fun x -> x) in
  (Gl.triangle_fan, vertices, colors, indices)

module Background = struct
  type t = {geometry: Gl_geometry.t; shader: Gl_shader.t}

  let create themes proj =
    match Themes.background themes with
    | Themes.Color c ->
        let r, g, b = color_to_floats c in
        Gl.clear_color r g b 1. ;
        let vertices =
          [|-2.; -1.; 0.; 11.; -1.; 0.; 11.; 4.; 0.; -2.; 4.; 0.|]
        in
        let colors =
          Array.init (Array.length vertices) (fun i ->
              match i mod 3 with 0 -> r | 1 -> g | _ -> b )
        in
        let indices = [|0; 1; 2; 2; 3; 0|] in
        let* geometry =
          Gl_geometry.of_arrays (Gl.lines, vertices, colors, indices)
        in
        let* shader = Gl_shader.create themes.data_path ["vertex"; "color"] in
        Gl_shader.send_matrix shader "view" proj ;
        Ok {geometry; shader}
    | Themes.Texture {name; x; y; w; h} ->
        let obj = text_rectangle (-2. +. x) (-1. +. y) w h in
        let* texture = Themes.prepend_path themes name in
        let frag_kind = `Textured in
        let* geometry = Gl_geometry.of_arrays ~frag_kind ~texture obj in
        let v_filename = "shaders/textured.vert" in
        let f_filename = "shaders/textured.frag" in
        let* shader =
          Gl_shader.create themes.data_path ~v_filename ~f_filename
            ["vertex"; "texture_coords"]
        in
        Gl_shader.send_matrix shader "view" proj ;
        Ok {geometry; shader}

  let draw t = Gl_geometry.draw t.shader.pid t.geometry

  let delete t =
    Gl_geometry.delete t.geometry ;
    Gl_shader.delete t.shader
end

module Cup = struct
  type r =
    { empty: Gl_geometry.t
    ; fallen: Gl_geometry.t
    ; full: Gl_geometry.t
    ; shader: Gl_shader.t }

  type t = r option

  let load_cup themes Themes.{name; x; y; w; h} =
    let obj = text_rectangle (-2. +. x) (-1. +. y) w h in
    let* texture = Themes.prepend_path themes name in
    let frag_kind = `Textured in
    Gl_geometry.of_arrays ~frag_kind ~texture obj

  let create themes proj =
    match Themes.dice_style themes with
    | Themes.Old ->
        Result.ok None
    | Themes.Animated {empty_cup; fallen_cup; full_cup; _} ->
        let v_filename = "shaders/textured.vert" in
        let f_filename = "shaders/textured.frag" in
        let* shader =
          Gl_shader.create themes.data_path ~v_filename ~f_filename
            ["vertex"; "texture_coords"]
        in
        let* fallen = load_cup themes fallen_cup in
        let* empty = load_cup themes empty_cup in
        let* full = load_cup themes full_cup in
        Gl_shader.send_matrix shader "view" proj ;
        Result.ok (Some {fallen; empty; full; shader})

  let draw kind t =
    match (t, kind) with
    | None, _ ->
        ()
    | Some t, `Empty ->
        Gl_geometry.draw t.shader.pid t.empty
    | Some t, `Full ->
        Gl_geometry.draw t.shader.pid t.full
    | Some t, `Fallen ->
        Gl_geometry.draw t.shader.pid t.fallen

  let delete = function
    | None ->
        ()
    | Some t ->
        Gl_geometry.delete t.empty ;
        Gl_geometry.delete t.full ;
        Gl_geometry.delete t.fallen ;
        Gl_shader.delete t.shader
end

module Board = struct
  type t = {geometry: Gl_geometry.t; shader: Gl_shader.t}

  let create themes proj =
    (* Allow changing themes after launch *)
    match Themes.board themes with
    | Themes.Color c ->
        let r, g, b = color_to_floats c in
        let vertices =
          Array.init
            (9 * 4 * 3)
            (fun i ->
              let c = i / 3 in
              let y = c / 9 in
              let x = c mod 9 in
              match i mod 3 with 0 -> float x | 1 -> float y | _ -> 0. )
        in
        let colors =
          Array.init (Array.length vertices) (fun i ->
              match i mod 3 with 0 -> r | 1 -> g | _ -> b )
        in
        let c x y = x + (9 * y) in
        let indices =
          [| (* Horizontal : bas en haut *)
             c 0 0
           ; c 4 0
           ; c 6 0
           ; c 8 0
           ; c 0 1
           ; c 8 1
           ; c 0 2
           ; c 8 2
           ; c 0 3
           ; c 4 3
           ; c 6 3
           ; c 8 3
           ; (* Vertical : gauche à droite *)
             c 0 0
           ; c 0 3
           ; c 1 0
           ; c 1 3
           ; c 2 0
           ; c 2 3
           ; c 3 0
           ; c 3 3
           ; c 4 0
           ; c 4 3
           ; c 5 1
           ; c 5 2
           ; c 6 0
           ; c 6 3
           ; c 7 0
           ; c 7 3
           ; c 8 0
           ; c 8 3
           ; (* Rosaces : haut en bas*)
             c 0 0
           ; c 1 1
           ; c 0 1
           ; c 1 0
           ; c 6 0
           ; c 7 1
           ; c 6 1
           ; c 7 0
           ; c 3 1
           ; c 4 2
           ; c 3 2
           ; c 4 1
           ; c 0 2
           ; c 1 3
           ; c 0 3
           ; c 1 2
           ; c 6 2
           ; c 7 3
           ; c 6 3
           ; c 7 2 |]
        in
        let* geometry =
          Gl_geometry.of_arrays (Gl.lines, vertices, colors, indices)
        in
        let* shader = Gl_shader.create themes.data_path ["vertex"; "color"] in
        Gl_shader.send_matrix shader "view" proj ;
        Ok {geometry; shader}
    | Themes.Texture {name; x; y; w; h} ->
        let obj = text_rectangle (-2. +. x) (-1. +. y) w h in
        let* texture = Themes.prepend_path themes name in
        let frag_kind = `Textured in
        let* geometry = Gl_geometry.of_arrays ~frag_kind ~texture obj in
        let v_filename = "shaders/textured.vert" in
        let f_filename = "shaders/textured.frag" in
        let* shader =
          Gl_shader.create themes.data_path ~v_filename ~f_filename
            ["vertex"; "texture_coords"]
        in
        Gl_shader.send_matrix shader "view" proj ;
        Ok {geometry; shader}

  let draw t = Gl_geometry.draw t.shader.pid t.geometry

  let delete t =
    Gl_geometry.delete t.geometry ;
    Gl_shader.delete t.shader
end

module Dice = struct
  type old = {base: Gl_geometry.t; cap: Gl_geometry.t; shader: Gl_shader.t}

  type textured =
    {shader: Gl_shader.t; dice_1: Gl_geometry.t list; dice_2: Gl_geometry.t list}

  type t = Old of old | New of textured

  let load_dice themes Themes.{name; x; y; w; h} =
    let obj = text_rectangle x y w h in
    let* texture = Themes.prepend_path themes name in
    let frag_kind = `Textured in
    Gl_geometry.of_arrays ~frag_kind ~texture obj

  let create themes proj =
    match Themes.dice_style themes with
    | Themes.Old ->
        let* base = Gl_geometry.of_arrays @@ triangle 0. 0. 0. in
        let* cap = Gl_geometry.of_arrays @@ triangle 1. 1. 1. in
        let* shader = Gl_shader.create themes.data_path ["vertex"; "color"] in
        Gl_shader.send_matrix shader "view" proj ;
        Ok (Old {base; cap; shader})
    | Themes.Animated {dice_1; dice_2; _} ->
        let v_filename = "shaders/textured.vert" in
        let f_filename = "shaders/textured.frag" in
        let* shader =
          Gl_shader.create themes.data_path ~v_filename ~f_filename
            ["vertex"; "texture_coords"]
        in
        let* dice_1 =
          List.map (load_dice themes) dice_1 |> flatten_result_list
        in
        let* dice_2 =
          List.map (load_dice themes) dice_2 |> flatten_result_list
        in
        Gl_shader.send_matrix shader "view" proj ;
        Ok (New {shader; dice_1; dice_2})

  let base_factor = 0.2

  let cap_factor = 0.05

  let is_on n = n <= 2

  let draw t ~n ~dice_n ~x ~y =
    match t with
    | Old t ->
        let pid = t.shader.pid in
        let trans =
          Matrix.translation x y 0. |> Matrix.scale base_factor base_factor 0.
        in
        Gl_geometry.draw ~trans pid t.base ;
        if is_on n then
          let trans =
            Matrix.translation x (y +. (base_factor -. cap_factor)) 0.
            |> Matrix.scale cap_factor cap_factor 0.
          in
          Gl_geometry.draw ~trans pid t.cap
    | New t ->
        let pid = t.shader.pid in
        let trans = Matrix.translation x y 0. in
        let d = if dice_n = 1 then t.dice_1 else t.dice_2 in
        let g = List.nth d n in
        Gl_geometry.draw ~trans pid g

  let delete = function
    | Old t ->
        Gl_geometry.delete t.base ;
        Gl_geometry.delete t.cap ;
        Gl_shader.delete t.shader
    | New _ ->
        ()
end

module Pawn = struct
  type t =
    { p1: Gl_geometry.t
    ; p2: Gl_geometry.t
    ; p1a: Gl_geometry.t
    ; p2a: Gl_geometry.t
    ; c: Gl_geometry.t
    ; s1: Gl_shader.t
    ; s2: Gl_shader.t
    ; s1a: Gl_shader.t
    ; s2a: Gl_shader.t
    ; sc: Gl_shader.t }

  let geom_from_sum themes =
    let open Themes in
    function
    | Color c ->
        let r, g, b = color_to_floats c in
        let* geom = Gl_geometry.of_arrays @@ circle r g b 200 0.4 in
        let* shader = Gl_shader.create themes.data_path ["vertex"; "color"] in
        Result.ok (geom, shader)
    | Texture {name; x; y; w; h} ->
        let obj = text_rectangle (-0.5 +. x) (-0.5 +. y) w h in
        let* texture = Themes.prepend_path themes name in
        let frag_kind = `Textured in
        let* geom = Gl_geometry.of_arrays ~frag_kind ~texture obj in
        let v_filename = "shaders/textured.vert" in
        let f_filename = "shaders/textured.frag" in
        let* shader =
          Gl_shader.create themes.data_path ~v_filename ~f_filename
            ["vertex"; "texture_coords"]
        in
        Result.ok (geom, shader)

  let create themes proj =
    let* p1, s1 = geom_from_sum themes @@ Themes.p1_pawn themes in
    let* p2, s2 = geom_from_sum themes @@ Themes.p2_pawn themes in
    let* p1a, s1a = geom_from_sum themes @@ Themes.p1_pawn_alt themes in
    let* p2a, s2a = geom_from_sum themes @@ Themes.p2_pawn_alt themes in
    let* c, sc = geom_from_sum themes @@ Themes.hollow_pawn themes in
    Gl_shader.send_matrix s1 "view" proj ;
    Gl_shader.send_matrix s2 "view" proj ;
    Gl_shader.send_matrix s1a "view" proj ;
    Gl_shader.send_matrix s2a "view" proj ;
    Gl_shader.send_matrix sc "view" proj ;
    Ok {p1; p2; p1a; p2a; c; s1; s2; s1a; s2a; sc}

  let pawn_to_float pawn =
    let pawn_to_coord = function
      | {Game.Logic.owner= P1; position= Intro x} ->
          (3 - x, 0)
      | {owner= P2; position= Intro x} ->
          (3 - x, 2)
      | {owner= P1; position= Outro x} ->
          (6 + (1 - x), 0)
      | {owner= P2; position= Outro x} ->
          (6 + (1 - x), 2)
      | {position= Main x; _} ->
          (x, 1)
      | {owner= P1; position= Reserve} ->
          (4, 0)
      | {owner= P2; position= Reserve} ->
          (4, 2)
    in
    let nx, ny = pawn_to_coord pawn in
    (float nx +. 0.5, float ny +. 0.5)

  let draw_reserve t ~x ~y n player =
    let p, s = if player = Game.P1 then (t.p1, t.s1) else (t.p2, t.s2) in
    for i = 0 to n - 1 do
      let trans =
        Matrix.translation (x +. (float i *. 0.23)) y 0.
        |> Matrix.scale 0.2 0.2 0.
      in
      Gl_geometry.draw ~trans s.pid p
    done

  let draw t ?animate ?choice pawn =
    (* Grille 8 x 3 *)
    let p, s =
      match choice with
      | None ->
          if Game.Logic.(pawn.owner) = P1 then (t.p1, t.s1) else (t.p2, t.s2)
      | Some (`Empty, _) ->
          (t.c, t.sc)
      | Some (`Full, _) ->
          if Game.Logic.(pawn.owner) = P1 then (t.p1a, t.s1a) else (t.p2a, t.s2a)
    in
    let def_scale =
      match choice with None -> 1. | Some (_, x) -> 1.5 -. (x /. 2.)
    in
    let x, y, prog =
      match animate with
      | None ->
          let x, y = pawn_to_float pawn in
          (x, y, 0.)
      | Some (pawn', prog) ->
          let prog' = smooth_progress prog in
          let x1, y1 = pawn_to_float pawn in
          let x2, y2 = pawn_to_float pawn' in
          (x1 +. ((x2 -. x1) *. prog'), y1 +. ((y2 -. y1) *. prog'), prog)
    in
    let scafa = 1. +. (cos ((pi /. 2.) +. (pi *. prog)) /. 3.) in
    let trans =
      Matrix.translation x y 0.
      |> Matrix.scale def_scale def_scale 0.
      |> Matrix.scale scafa scafa 0.
    in
    Gl_geometry.draw ~trans s.pid p

  let delete t =
    Gl_geometry.delete t.p1 ;
    Gl_geometry.delete t.p2 ;
    Gl_geometry.delete t.p1a ;
    Gl_geometry.delete t.p2a ;
    Gl_geometry.delete t.c ;
    Gl_shader.delete t.s1 ;
    Gl_shader.delete t.s2 ;
    Gl_shader.delete t.s1a ;
    Gl_shader.delete t.s2a ;
    Gl_shader.delete t.sc
end
