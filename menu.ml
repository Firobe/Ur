module Choice = struct
  type t = {header: string; options: (int * string list) option; final: bool}

  let get_text c =
    match c.options with
    | None ->
        c.header
    | Some (s, l) ->
        let v = List.nth l s in
        Printf.sprintf "< %s: %s >" c.header v

  let eq c1 c2 = c1.header = c2.header
end

type t = {choices: Choice.t list; highlighted: int}

let get_current_choice t = List.nth t.choices t.highlighted

let get_choice_number t c =
  let open Choice in
  let with_num = List.mapi (fun i x -> (i, x)) t.choices in
  let i, _ = List.find (fun (_, x) -> x.header = c.header) with_num in
  i

let get_nth_choice t n = List.nth t.choices n

let get_choice_option t choice =
  let open Choice in
  let ( let* ) = Option.bind in
  let* c = List.find_opt (fun c -> c.header = choice) t.choices in
  let* selected, options = c.options in
  List.nth_opt options selected

let modulo x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let move_highlighted t delta =
  let highlighted = modulo (t.highlighted + delta) (List.length t.choices) in
  {t with highlighted}

let move_option t delta =
  let cc = get_current_choice t in
  let choices =
    List.map
      (fun c ->
        if c = cc then
          match c.options with
          | None ->
              c
          | Some (s, l) ->
              let s' = modulo (s + delta) (List.length l) in
              {c with options= Some (s', l)}
        else c )
      t.choices
  in
  {t with choices}

let default_menu themes =
  { choices=
      [ {header= "How to play"; final= true; options= None}
      ; { header= "Red player"
        ; final= false
        ; options= Some (0, ["Human"; "AI (Random)"; "AI (Smart)"]) }
      ; { header= "Blue player"
        ; final= false
        ; options= Some (0, ["Human"; "AI (Random)"; "AI (Smart)"]) }
      ; { header= "Pawns"
        ; final= false
        ; options= Some (3, ["1"; "3"; "5"; "7"; "9"; "15"; "30"]) }
      ; {header= "Theme"; final= false; options= Some (Themes.to_menu themes)}
      ; { header= "Game speed"
        ; final= false
        ; options= Some (1, ["50"; "100"; "200"; "500"; "1000"; "5000"]) }
      ; {header= "Play !"; final= true; options= None} ]
  ; highlighted= 0 }
