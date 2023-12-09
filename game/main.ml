module L = Lib
module R = Raylib

type game_state =
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
    | false -> draw_texture highlight x y Color.raywhite
    | true -> game_state_global := MENU

let highlight_button_menu mouse hitbox highlight (x, y) n =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        game := Some (L.Game.init n);
        game_state_global := ACTIVE

let highlight_button_state mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        game := None;
        game_state_global := END

let highlight_button_phase mouse hitbox highlight (x, y) =
  let open R in
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_pressed MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true -> game := Some (L.Game.change_phase (Option.get !game))

let draw_territories_of_player (player : L.Player.t) =
  let open R in
  let open L in
  let territories = L.Player.get_territories player in
  Array.iter
    (fun elem ->
      match elem with
      | None -> ()
      | Some territory ->
          let location = Territories.get_location territory in
          draw_text
            (string_of_int (Territories.get_troops territory))
            (fst location) (snd location) 20 (Player.get_color player))
    territories

(* Game setup function *)
let setup () =
  let open R in
  let open L in
  init_window Constants.screen_width L.Constants.screen_height "risk_of_ocaml";
  set_target_fps Constants.screen_fps;
  Start.initialize_start ();
  Menu.initialize_menu ();
  Active.initialize_active ();
  ()

(*Game loop function*)
let rec loop game_state =
  let open R in
  match window_should_close () with
  | true -> close_window ()
  | false -> (
      let open L in
      match game_state with
      | START ->
          begin_drawing ();
          clear_background R.Color.raywhite;
          Start.draw_start ();

          (* mouse detection on START button *)
          let mouse = get_mouse_position () in
          highlight_button_start mouse start_hb (L.Start.get_button_hl ())
            (480, 570);

          end_drawing ();
          loop !game_state_global
      | MENU ->
          begin_drawing ();
          clear_background R.Color.raywhite;
          Menu.draw_menu ();

          (* mouse detection on player buttons *)
          let mouse = get_mouse_position () in
          let two, three, four, five, six = Menu.get_hl () in
          highlight_button_menu mouse two_pb_hb two (225, 350) 2;
          highlight_button_menu mouse three_pb_hb three (550, 350) 3;
          highlight_button_menu mouse four_pb_hb four (875, 350) 4;
          highlight_button_menu mouse five_pb_hb five (388, 550) 5;
          highlight_button_menu mouse six_pb_hb six (713, 550) 6;

          end_drawing ();
          loop !game_state_global
      | ACTIVE ->
          begin_drawing ();
          clear_background R.Color.raywhite;
          Active.draw_active ();

          (* draw current player *)
          let curr_player = Game.get_current_player (Option.get !game) in
          let curr_player_name = Player.get_name curr_player in
          let curr_player_color = Player.get_color curr_player in
          let curr_player_string =
            "It is Player " ^ curr_player_name ^ "'s turn"
          in
          draw_text curr_player_string 378 811 50 curr_player_color;

          (* draw the territory textboxes on the screen *)
          let players = Game.get_players (Option.get !game) in
          (* for every player in the current game: for every territory in that
             player's option array: check whether the territory is Some or None;
             if Some territory, then get the territory's location tuple; draw
             the territory text box on the screen with the player's associated
             color *)
          let _ =
            List.iter (fun player -> draw_territories_of_player player) players
          in
          ();
          let state, phase = Active.get_hl () in
          let mouse = get_mouse_position () in
          (* change state *)
          highlight_button_state mouse state_hb state (300, 350);
          (* change phase *)
          highlight_button_phase mouse phase_hb phase (750, 350);

          end_drawing ();
          loop !game_state_global
      | END ->
          game_state_global := START;
          loop !game_state_global)

(* initializes game *)
let _ =
  setup ();
  loop !game_state_global
