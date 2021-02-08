open Sexplib
open Sexplib.Std

type color = int * int * int [@@deriving sexp]

type text_colors =
  {base: color; menu_selected: color; alert: color; p1: color; p2: color}
[@@deriving sexp]

type texture =
  { name: string
  ; x: float [@default 0.]
  ; y: float [@default 0.]
  ; w: float [@default 1.]
  ; h: float [@default 1.] }
[@@deriving sexp]

type color_or_texture = Color of color | Texture of texture [@@deriving sexp]

type animated_throw =
  {empty_cup: texture; fallen_cup: texture; full_cup: texture}
[@@deriving sexp]

type dice_style = Old | Animated of animated_throw [@@deriving sexp]

type theme =
  { background: color_or_texture
  ; text_colors: text_colors
  ; font: string
  ; board: color_or_texture
  ; dice_style: dice_style
  ; p1_pawn: color_or_texture
  ; p2_pawn: color_or_texture
  ; p1_pawn_alt: color_or_texture
  ; p2_pawn_alt: color_or_texture
  ; hollow_pawn: color_or_texture }
[@@deriving sexp]

type t = {themes: (string * theme) list; selected: string}

let themes_dir = "themes"

let load_theme dir name =
  let dest = Printf.sprintf "%s/%s/" dir name in
  let s = Sexp.load_sexp (dest ^ "theme.scm") in
  let t = theme_of_sexp s in
  (name, t)

let load_themes () =
  let dir = themes_dir in
  try
    if Sys.is_directory dir then
      let dest s = Printf.sprintf "%s/%s/" dir s in
      let themes =
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun s -> Sys.is_directory (dest s))
        |> List.map (load_theme dir) in
      {themes; selected= "naya"}
    else failwith "Invalid themes directory"
  with Sys_error s -> failwith ("Error while loading themes: " ^ s)

let to_menu t =
  let names = List.map fst t.themes in
  let n =
    List.mapi (fun i x -> (i, x)) names
    |> List.find (fun (_, x) -> x = "naya")
    |> fst in
  (n, names)

let current t = List.assoc t.selected t.themes
let prepend_path t path = Printf.sprintf "%s/%s/%s" themes_dir t.selected path
let font t = (current t).font |> prepend_path t
let background t = (current t).background
let p1_pawn t = (current t).p1_pawn
let p2_pawn t = (current t).p2_pawn
let p1_pawn_alt t = (current t).p1_pawn_alt
let p2_pawn_alt t = (current t).p2_pawn_alt
let hollow_pawn t = (current t).hollow_pawn
let text_colors t = (current t).text_colors
let board t = (current t).board
let dice_style t = (current t).dice_style
let selected t = t.selected
