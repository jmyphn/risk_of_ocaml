module L = Lib
module R = Raylib

type state =
  | START
  | MENU
  | ACTIVE
  | END

let game_state_global = ref START
let game = ref None

(* player button hitboxes *)
let start_hb = R.Rectangle.create 480. 570. 240. 100.
let two_pb_hb = R.Rectangle.create 225. 350. 100. 100.
let three_pb_hb = R.Rectangle.create 550. 350. 100. 100.
let four_pb_hb = R.Rectangle.create 875. 350. 100. 100.
let five_pb_hb = R.Rectangle.create 388. 550. 100. 100.
let six_pb_hb = R.Rectangle.create 713. 550. 100. 100.

(* state temp button hitbox *)
let state_hb = R.Rectangle.create 300. 350. 150. 100.

(* phase temp button hitbox *)
let phase_hb = R.Rectangle.create 750. 350. 150. 100.

let highlight_button_start mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> R.draw_texture highlight x y Color.raywhite
    | true -> game_state_global := MENU

let highlight_button_menu mouse hitbox highlight (x, y) n =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> R.draw_texture highlight x y Color.raywhite
    | true ->
        game := Some (L.Game.init n);
        game_state_global := ACTIVE

let highlight_button_state mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> R.draw_texture highlight x y Color.raywhite
    | true ->
        game := None;
        game_state_global := END

let highlight_button_phase mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_pressed MouseButton.Left with
    | false -> R.draw_texture highlight x y Color.raywhite
    | true -> game := Some (L.Game.change_phase (Option.get !game))

let draw_territories_of_player (player : L.Player.t) =
  let territories = L.Player.get_territories player in
  Array.iter
    (fun elem ->
      match elem with
      | None -> ()
      | Some territory ->
          let location = L.Territories.get_location territory in
          R.draw_text
            (string_of_int (L.Territories.get_troops territory))
            (fst location - 20)
            (snd location) 20
            (L.Player.get_color player))
    territories

(*Game setup function*)
let setup () =
  let open R in
  init_window 1400 900 "risk_of_ocaml";
  set_target_fps 60;

  let bg_start = load_image "assets/start/StartBackground.png" in
  let bg_start_texture = load_texture_from_image bg_start in
  unload_image bg_start;

  (* menu *)
  let bg_menu = load_image "assets/menu/MenuBackground.png" in
  let bg_menu_texture = load_texture_from_image bg_menu in
  unload_image bg_menu;

  (* state button *)
  let bg_active = load_image "assets/MapBackground.png" in
  let bg_active_texture = load_texture_from_image bg_active in
  unload_image bg_active;

  let bg_active_button_tmp = load_image "assets/TempButton.png" in
  let bg_active_button_texture = load_texture_from_image bg_active_button_tmp in
  unload_image bg_active_button_tmp;

  let bg_active_button_tmp_highlight =
    load_image "assets/TempButtonHighlight.png"
  in
  let bg_active_button_texture_highlight =
    load_texture_from_image bg_active_button_tmp_highlight
  in
  unload_image bg_active_button_tmp_highlight;

  (* phase button *)
  let bg_phase_button_tmp = load_image "assets/PhaseButton.png" in
  let bg_phase_button_texture = load_texture_from_image bg_phase_button_tmp in
  unload_image bg_phase_button_tmp;

  let bg_phase_button_tmp_highlight =
    load_image "assets/PhaseButtonHighlight.png"
  in
  let bg_phase_button_texture_highlight =
    load_texture_from_image bg_phase_button_tmp_highlight
  in
  unload_image bg_phase_button_tmp_highlight;

  (* end *)
  (* let bg_end = load_image "assets/EndBackground.png" in let bg_end_texture =
     load_texture_from_image bg_end in unload_image bg_end; *)

  (* Start button *)
  let start_button = load_image "assets/start/StartButton.png" in
  let start_button_texture = load_texture_from_image start_button in
  unload_image start_button;
  let start_button_highlight =
    load_image "assets/start/StartButtonHighlight.png"
  in
  let start_button_highlight_texture =
    load_texture_from_image start_button_highlight
  in
  unload_image start_button_highlight;

  (* Player count buttons *)
  let two_pb = load_image "assets/menu/2PB.png" in
  let two_pb_texture = load_texture_from_image two_pb in
  unload_image two_pb;
  let two_pb_highlight = load_image "assets/menu/2PBHighlight.png" in
  let two_pb_highlight_texture = load_texture_from_image two_pb_highlight in
  unload_image two_pb_highlight;

  let three_pb = load_image "assets/menu/3PB.png" in
  let three_pb_texture = load_texture_from_image three_pb in
  unload_image three_pb;
  let three_pb_highlight = load_image "assets/menu/3PBHighlight.png" in
  let three_pb_highlight_texture = load_texture_from_image three_pb_highlight in
  unload_image three_pb_highlight;

  let four_pb = load_image "assets/menu/4PB.png" in
  let four_pb_texture = load_texture_from_image four_pb in
  unload_image four_pb;
  let four_pb_highlight = load_image "assets/menu/4PBHighlight.png" in
  let four_pb_highlight_texture = load_texture_from_image four_pb_highlight in
  unload_image four_pb_highlight;

  let five_pb = load_image "assets/menu/5PB.png" in
  let five_pb_texture = load_texture_from_image five_pb in
  unload_image five_pb;
  let five_pb_highlight = load_image "assets/menu/5PBHighlight.png" in
  let five_pb_highlight_texture = load_texture_from_image five_pb_highlight in
  unload_image five_pb_highlight;

  (* active state *)
  let six_pb = load_image "assets/menu/6PB.png" in
  let six_pb_texture = load_texture_from_image six_pb in
  unload_image six_pb;
  let six_pb_highlight = load_image "assets/menu/6PBHighlight.png" in
  let six_pb_highlight_texture = load_texture_from_image six_pb_highlight in
  unload_image six_pb_highlight;

  ( bg_start_texture,
    bg_menu_texture,
    bg_active_texture,
    bg_active_button_texture,
    bg_active_button_texture_highlight,
    bg_phase_button_texture,
    bg_phase_button_texture_highlight,
    (* bg_end_texture, *)
    start_button_texture,
    start_button_highlight_texture,
    two_pb_texture,
    two_pb_highlight_texture,
    three_pb_texture,
    three_pb_highlight_texture,
    four_pb_texture,
    four_pb_highlight_texture,
    five_pb_texture,
    five_pb_highlight_texture,
    six_pb_texture,
    six_pb_highlight_texture )

(*Game loop function*)
let rec loop game_state textures =
  let ( start,
        menu,
        active,
        active_button,
        active_button_highlight,
        phase_button,
        phase_button_highlight,
        (* enderman, *)
        start_button,
        start_button_highlight,
        two_pb_button,
        two_pb_button_highlight,
        three_pb_button,
        three_pb_button_highlight,
        four_pb_button,
        four_pb_button_highlight,
        five_pb_button,
        five_pb_button_highlight,
        six_pb_button,
        six_pb_button_highlight ) =
    textures
  in
  match R.window_should_close () with
  | true -> R.close_window ()
  | false -> (
      match game_state with
      | START ->
          R.begin_drawing ();
          R.clear_background R.Color.raywhite;
          R.draw_texture start 0 0 R.Color.raywhite;
          R.draw_texture start_button 480 570 R.Color.raywhite;

          (* mouse detection on START button *)
          let mouse = R.get_mouse_position () in
          highlight_button_start mouse start_hb start_button_highlight (480, 570);

          R.end_drawing ();
          loop !game_state_global textures
      | MENU ->
          R.begin_drawing ();
          R.clear_background R.Color.raywhite;
          R.draw_texture menu 0 0 R.Color.raywhite;
          R.draw_texture two_pb_button 225 350 R.Color.raywhite;
          R.draw_texture three_pb_button 550 350 R.Color.raywhite;
          R.draw_texture four_pb_button 875 350 R.Color.raywhite;
          R.draw_texture five_pb_button 388 550 R.Color.raywhite;
          R.draw_texture six_pb_button 713 550 R.Color.raywhite;

          (* mouse detection on player buttons *)
          let mouse = R.get_mouse_position () in
          highlight_button_menu mouse two_pb_hb two_pb_button_highlight
            (225, 350) 2;
          highlight_button_menu mouse three_pb_hb three_pb_button_highlight
            (550, 350) 3;
          highlight_button_menu mouse four_pb_hb four_pb_button_highlight
            (875, 350) 4;
          highlight_button_menu mouse five_pb_hb five_pb_button_highlight
            (388, 550) 5;
          highlight_button_menu mouse six_pb_hb six_pb_button_highlight
            (713, 550) 6;

          R.end_drawing ();
          loop !game_state_global textures
      | ACTIVE ->
          R.begin_drawing ();
          R.clear_background R.Color.raywhite;

          (* making active state *)

          (* draw the map itself *)
          R.draw_texture active 0 0 R.Color.raywhite;

          (* draw current player *)
          let curr_player = L.Game.get_current_player (Option.get !game) in
          let curr_player_name = L.Player.get_name curr_player in
          let curr_player_color = L.Player.get_color curr_player in
          let curr_player_string =
            "It is Player " ^ curr_player_name ^ "'s turn"
          in
          R.draw_text curr_player_string 378 811 50 curr_player_color;

          (* draw the territory textboxes on the screen *)
          let players = L.Game.get_players (Option.get !game) in
          (* for every player in the current game: for every territory in that
             player's option array: check whether the territory is Some or None;
             if Some territory, then get the territory's location tuple; draw
             the territory text box on the screen with the player's associated
             color *)
          let _ =
            List.iter (fun player -> draw_territories_of_player player) players
          in
          ();

          (* change state *)
          R.draw_texture active_button 300 350 R.Color.raywhite;
          let mouse = R.get_mouse_position () in
          highlight_button_state mouse state_hb active_button_highlight
            (300, 350);

          (* change phase *)
          R.draw_texture phase_button 750 350 R.Color.raywhite;
          highlight_button_phase mouse phase_hb phase_button_highlight (750, 350);

          R.end_drawing ();
          loop !game_state_global textures
      | END ->
          game_state_global := START;
          loop !game_state_global textures)

(*initializes game*)
let () = setup () |> loop !game_state_global
