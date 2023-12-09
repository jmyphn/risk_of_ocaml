open Raylib

type t = {
  bg : Texture2D.t;
  button : Texture2D.t;
  button_hl : Texture2D.t;
}

let start = ref None

let initialize_start () =
  let bg_start_texture = load_texture "assets/start/StartBackground.png" in
  let start_button_texture = load_texture "assets/start/StartButton.png" in
  let start_button_highlight_texture =
    load_texture "assets/start/StartButtonHighlight.png"
  in
  start :=
    Some
      {
        bg = bg_start_texture;
        button = start_button_texture;
        button_hl = start_button_highlight_texture;
      }

let get_button_hl () =
  match !start with
  | None -> failwith "Failed to load in start textures"
  | Some s -> s.button_hl

let draw_start () =
  let start = Option.get !start in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro start.bg source dest origin 0. Constants.default_color;
  draw_texture start.button 480 570 Constants.default_color
