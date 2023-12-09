open Lib
open Raylib

(* state temp button hitbox *)
let state_hb = Rectangle.create 300. 350. 150. 100.

(* phase temp button hitbox *)
let phase_hb = Rectangle.create 750. 350. 150. 100.

let highlight_button_state mouse hitbox highlight (x, y) =
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_down MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        Constants.game_active := None;
        Constants.game_state := END

let highlight_button_phase mouse hitbox highlight (x, y) =
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_pressed MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        Constants.game_active :=
          Some (Game.change_phase (Constants.get_game ()))

let draw_territories_of_player (player : Player.t) =
  let territories = Player.get_territories player in
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
  init_window Constants.screen_width Constants.screen_height "risk_of_ocaml";
  set_target_fps Constants.screen_fps;
  Start.initialize_start ();
  Menu.initialize_menu ();
  Instructions.initialize_instructions ();
  Active.initialize_active ();
  ()

(* Game loop function *)
let rec loop () =
  match window_should_close () with
  | true -> close_window ()
  | false -> (
      match Constants.get_state () with
      | START ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Start.draw_start mouse;
          end_drawing ();
          loop ()
      | MENU ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Menu.draw_menu mouse;
          end_drawing ();
          loop ()
      | INSTRUCTIONS ->
          begin_drawing ();
          clear_background Constants.default_color;
          let mouse = get_mouse_position () in
          Instructions.draw_instructions mouse;
          end_drawing ();
          loop ()
      | ACTIVE ->
          begin_drawing ();
          clear_background Color.raywhite;
          Active.draw_active ();
          let game = Constants.get_game () in

          (* draw current player *)
          let curr_player = Game.get_current_player game in
          let curr_player_name = Player.get_name curr_player in
          let curr_player_color = Player.get_color curr_player in
          let curr_player_string =
            "It is Player " ^ curr_player_name ^ "'s turn"
          in
          draw_text curr_player_string 378 811 50 curr_player_color;

          (* draw the territory textboxes on the screen *)
          let players = Game.get_players game in
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
          loop ()
      | END ->
          Constants.game_state := START;
          loop ())

(* initializes game *)
let _ =
  setup ();
  loop ()
