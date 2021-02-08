open Sexplib
open Sexplib.Std

type color = int * int * int [@@deriving sexp]

type text_colors =
  {base: color; menu_selected: color; alert: color; p1: color; p2: color}
[@@deriving sexp]

type theme =
  { background_color: color
  ; text_colors: text_colors
  ; font: string
  ; board_texture: string
  ; pawn_texture: string }
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
      {themes; selected= "default"}
    else failwith "Invalid themes directory"
  with Sys_error s -> failwith ("Error while loading themes: " ^ s)

let to_menu t =
  let names = List.map fst t.themes in
  let n =
    List.mapi (fun i x -> (i, x)) names
    |> List.find (fun (_, x) -> x = "default")
    |> fst in
  (n, names)

let selected t = List.assoc t.selected t.themes
let prepend_path name path = Printf.sprintf "%s/%s/%s" themes_dir name path
let font t = (selected t).font |> prepend_path t.selected
let background_color t = (selected t).background_color
let text_colors t = (selected t).text_colors
