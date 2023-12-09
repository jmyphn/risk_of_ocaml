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
let two_pb_hb = Rectangle.create 225. 350. 100. 100.
let three_pb_hb = Rectangle.create 550. 350. 100. 100.
let four_pb_hb = Rectangle.create 875. 350. 100. 100.
let five_pb_hb = Rectangle.create 388. 550. 100. 100.
let six_pb_hb = Rectangle.create 713. 550. 100. 100.

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

let highlight_button_menu mouse hitbox highlight (x, y) n =
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        Constants.game_active := Some (Game.init n);
        Constants.game_state := ACTIVE

let draw_menu mouse =
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
  draw_texture menu.six_pb 713 550 Constants.default_color;
  highlight_button_menu mouse two_pb_hb menu.two_pb_hl (225, 350) 2;
  highlight_button_menu mouse three_pb_hb menu.three_pb_hl (550, 350) 3;
  highlight_button_menu mouse four_pb_hb menu.four_pb_hl (875, 350) 4;
  highlight_button_menu mouse five_pb_hb menu.five_pb_hl (388, 550) 5;
  highlight_button_menu mouse six_pb_hb menu.six_pb_hl (713, 550) 6
