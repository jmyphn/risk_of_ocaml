open Raylib

type t = {
  bg : Texture2D.t;
  two_pb : Texture2D.t;
  two_pb_hl : Texture2D.t;
  three_pb : Texture2D.t;
  three_pb_hl : Texture2D.t;
  four_pb : Texture2D.t;
  four_pb_hl : Texture2D.t;
  five_pb : Texture2D.t;
  five_pb_hl : Texture2D.t;
  six_pb : Texture2D.t;
  six_pb_hl : Texture2D.t;
}

let menu = ref None

let initialize_menu () =
  let bg_menu_texture = load_texture "assets/menu/MenuBackground.png" in
  let two_pb_texture = load_texture "assets/menu/2PB.png" in
  let two_pb_highlight_texture = load_texture "assets/menu/2PBHighlight.png" in

  let three_pb_texture = load_texture "assets/menu/3PB.png" in
  let three_pb_highlight_texture =
    load_texture "assets/menu/3PBHighlight.png"
  in

  let four_pb_texture = load_texture "assets/menu/4PB.png" in
  let four_pb_highlight_texture = load_texture "assets/menu/4PBHighlight.png" in

  let five_pb_texture = load_texture "assets/menu/5PB.png" in
  let five_pb_highlight_texture = load_texture "assets/menu/5PBHighlight.png" in

  (* active state *)
  let six_pb_texture = load_texture "assets/menu/6PB.png" in
  let six_pb_highlight_texture = load_texture "assets/menu/6PBHighlight.png" in
  menu :=
    Some
      {
        bg = bg_menu_texture;
        two_pb = two_pb_texture;
        two_pb_hl = two_pb_highlight_texture;
        three_pb = three_pb_texture;
        three_pb_hl = three_pb_highlight_texture;
        four_pb = four_pb_texture;
        four_pb_hl = four_pb_highlight_texture;
        five_pb = five_pb_texture;
        five_pb_hl = five_pb_highlight_texture;
        six_pb = six_pb_texture;
        six_pb_hl = six_pb_highlight_texture;
      }

let draw_menu () =
  let menu = Option.get !menu in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro menu.bg source dest origin 0. Constants.default_color;
  draw_texture menu.two_pb 225 350 Constants.default_color;
  draw_texture menu.three_pb 550 350 Constants.default_color;
  draw_texture menu.four_pb 875 350 Constants.default_color;
  draw_texture menu.five_pb 388 550 Constants.default_color;
  draw_texture menu.six_pb 713 550 Constants.default_color

let get_hl () =
  let res = Option.get !menu in
  (res.two_pb_hl, res.three_pb_hl, res.four_pb_hl, res.five_pb_hl, res.six_pb_hl)
