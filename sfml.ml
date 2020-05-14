type context = {
  window : SFRenderWindow.t;
  text : SFText.t
}

let draw_playing _game context =
  let open SFRenderWindow in
  clear context.window SFColor.green;
  drawText context.window ~text:context.text ();
  display context.window

let rec loop update_state state context =
  let state = update_state state in
  match state with
  | State.Playing game ->
    draw_playing game context;
    loop update_state state context
  | State.End ->
    Thread.delay 3.;
    Format.printf "End of display loop@."; context

let init () =
  let window = SFRenderWindow.make (800, 600) "Bonsoir" in
  let text = SFText.make "Hello SFML" in
  let font = SFFont.createFromFile ~filename:"./Compagnon-Light.otf" in
  SFText.setFont ~text ~font;
  {window; text}

let terminate context =
  SFRenderWindow.destroy context.window

let start update_state state =
  init () |> loop update_state state |> terminate
