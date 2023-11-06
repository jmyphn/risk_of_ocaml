module G = Lib
module R = Raylib

let mouse_state = ref 0
(* Mouse button during any state 0 -> Mouse is up 1 -> Mouse is down *)

let game_state_global = ref "start"
let start_hb = R.Rectangle.create 350. 500. 500. 200.

(*Game setup function*)
let setup () =
  let open R in
  init_window 1200 800 "risk_of_ocaml";
  set_target_fps 60;

  (* Background images for each state *)
  let bg_start = load_image "images/StartBackground.png" in
  let bg_start_texture = load_texture_from_image bg_start in
  unload_image bg_start;

  let bg_menu = load_image "images/MenuBackground.png" in
  let bg_menu_texture = load_texture_from_image bg_menu in
  unload_image bg_menu;

  (* let bg_active = load_image "images/MapBackground.png" in let
     bg_active_texture = load_texture_from_image bg_active in unload_image
     bg_active; let bg_end = load_image "images/EndBackground.png" in let
     bg_end_texture = load_texture_from_image bg_end in unload_image bg_end; *)

  (* Start button *)
  let start_button = load_image "images/StartButton.png" in
  let start_button_texture = load_texture_from_image start_button in
  unload_image start_button;
  let start_button_highlight = load_image "images/StartButtonHighlight.png" in
  let start_button_highlight_texture =
    load_texture_from_image start_button_highlight
  in
  unload_image start_button_highlight;
  (* Player count buttons *)
  let two_pb = load_image "images/2PB.png" in
  let two_pb_texture = load_texture_from_image two_pb in
  unload_image two_pb;
  let three_pb = load_image "images/3PB.png" in
  let three_pb_texture = load_texture_from_image three_pb in
  unload_image three_pb;
  let four_pb = load_image "images/4PB.png" in
  let four_pb_texture = load_texture_from_image four_pb in
  unload_image four_pb;
  let five_pb = load_image "images/5PB.png" in
  let five_pb_texture = load_texture_from_image five_pb in
  unload_image five_pb;
  let six_pb = load_image "images/6PB.png" in
  let six_pb_texture = load_texture_from_image six_pb in
  unload_image six_pb;
  ( bg_start_texture,
    bg_menu_texture,
    (* bg_active_texture, *)
    (* bg_end_texture, *)
    start_button_texture,
    start_button_highlight_texture,
    two_pb_texture,
    three_pb_texture,
    four_pb_texture,
    five_pb_texture,
    six_pb_texture )

let highlight_button mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false ->
        R.draw_texture highlight x y Color.raywhite
        (* mouse is hovering over the button*)
    | true -> game_state_global := "menu"
(* mouse pressed the button *)

(*Game loop function*)
let rec loop game_state textures =
  let ( start,
        menu,
        (* active, *)
        (* enderman, *)
        start_button,
        start_button_highlight,
        two_pb_button,
        three_pb_button,
        four_pb_button,
        five_pb_button,
        six_pb_button ) =
    textures
  in
  match R.window_should_close () with
  | true -> R.close_window ()
  | false -> (
      match game_state with
      | "start" ->
          R.begin_drawing ();
          R.clear_background R.Color.raywhite;
          R.draw_texture start 0 0 R.Color.raywhite;
          R.draw_texture start_button 350 500 R.Color.raywhite;
          if R.is_mouse_button_down R.MouseButton.Left = false then
            mouse_state := 0;
          let mouse = R.get_mouse_position () in
          highlight_button mouse start_hb start_button_highlight (350, 500);
          R.end_drawing ();
          loop !game_state_global textures
      | "menu" ->
          R.begin_drawing ();
          R.clear_background R.Color.raywhite;
          R.draw_texture menu 0 0 R.Color.raywhite;
          R.draw_texture two_pb_button 225 350 R.Color.raywhite;
          R.draw_texture three_pb_button 550 350 R.Color.raywhite;
          R.draw_texture four_pb_button 875 350 R.Color.raywhite;
          R.draw_texture five_pb_button 388 550 R.Color.raywhite;
          R.draw_texture six_pb_button 713 550 R.Color.raywhite;
          R.end_drawing ();
          loop !game_state_global textures
      | "active" -> loop !game_state_global textures
      | "end" -> loop !game_state_global textures
      | _ -> loop !game_state_global textures)

(*initializes game*)
let () = setup () |> loop !game_state_global
