open Sexplib
open Sexplib.Std
open Gl_utils

type color = int * int * int [@@deriving sexp]

type text_style = {color: color; outline: (int * color) option [@sexp.option]}
[@@deriving sexp]

type text_styles =
  { base: text_style
  ; menu_selected: text_style
  ; alert: text_style
  ; p1: text_style
  ; p2: text_style }
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
  { empty_cup: texture
  ; fallen_cup: texture
  ; full_cup: texture
  ; dice_1: texture list
  ; dice_2: texture list }
[@@deriving sexp]

type dice_style = Old | Animated of animated_throw [@@deriving sexp]

type sound_type =
  [ `title
  | `victory
  | `menu_choice
  | `menu_option
  | `select
  | `spawn
  | `eaten
  | `moving
  | `cup_full
  | `cup_thrown
  | `no_move ]
[@@deriving sexp]

type theme =
  { background: color_or_texture
  ; text_styles: text_styles
  ; font: string
  ; board: color_or_texture
  ; dice_style: dice_style
  ; p1_pawn: color_or_texture
  ; p2_pawn: color_or_texture
  ; p1_pawn_alt: color_or_texture
  ; rule_arrows: texture
  ; p2_pawn_alt: color_or_texture
  ; hollow_pawn: color_or_texture
  ; sounds: (sound_type * string * float) list [@sexp.list] }
[@@deriving sexp]

type t = {themes: (string * theme) list; selected: string; data_path: string}

let themes_dir = "themes"

let shared_dir = "common_assets"

let default_theme = "default"

let load_theme dir name =
  let dest = Printf.sprintf "%s/%s/" dir name in
  let s = Sexp.load_sexp (dest ^ "theme.scm") in
  try
    let t = theme_of_sexp s in
    (name, t)
  with e ->
    let mex = Printexc.to_string e in
    let msg =
      Printf.sprintf "Invalid theme description for '%s': %s" name mex
    in
    failwith msg

let load_themes share_path =
  let dir = Printf.sprintf "%s/%s" share_path themes_dir in
  try
    if Sys.is_directory dir then
      let dest s = Printf.sprintf "%s/%s/" dir s in
      let themes =
        Sys.readdir dir |> Array.to_list
        |> List.filter (fun s -> s <> shared_dir && Sys.is_directory (dest s))
        |> List.map (load_theme dir)
        |> List.sort (fun (a, _) (b, _) -> compare a b)
      in
      {themes; selected= default_theme; data_path= share_path}
    else failwith "Invalid themes directory"
  with Sys_error s -> failwith ("Error while loading themes: " ^ s)

let to_menu t =
  let names = List.map fst t.themes in
  let n =
    List.mapi (fun i x -> (i, x)) names
    |> List.find (fun (_, x) -> x = t.selected)
    |> fst
  in
  (n, names)

let current t = List.assoc t.selected t.themes

let prepend_path t path =
  let* where, path =
    match String.split_on_char '%' path with
    | [path] ->
        Result.ok (t.selected, path)
    | [""; "SHARED"; path] ->
        Result.ok (shared_dir, path)
    | _ ->
        Result.error (`Msg "Invalid use of %")
  in
  Result.ok (Printf.sprintf "%s/%s/%s/%s" t.data_path themes_dir where path)

let font t = (current t).font |> prepend_path t

let background t = (current t).background

let p1_pawn t = (current t).p1_pawn

let p2_pawn t = (current t).p2_pawn

let p1_pawn_alt t = (current t).p1_pawn_alt

let p2_pawn_alt t = (current t).p2_pawn_alt

let hollow_pawn t = (current t).hollow_pawn

let text_styles t = (current t).text_styles

let board t = (current t).board

let rule_arrows t = (current t).rule_arrows

let dice_style t = (current t).dice_style

let sounds t = (current t).sounds

let selected t = t.selected
