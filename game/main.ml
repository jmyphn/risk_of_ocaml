open Lib
open Raylib

(* state temp button hitbox *)
let state_hb = Rectangle.create 300. 350. 150. 100.

(* phase temp button hitbox *)
let phase_hb = Rectangle.create 750. 350. 150. 100.

let highlight_button_state mouse hitbox highlight (x, y) =
  if check_collision_point_rec mouse hitbox then
    match is_mouse_button_pressed MouseButton.Left with
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

(* Game setup function *)
let setup () =
  init_window Constants.screen_width Constants.screen_height "risk_of_ocaml";
  set_target_fps Constants.screen_fps;
  Start.initialize_start ();
  Menu.initialize_menu ();
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
      | ACTIVE ->
          begin_drawing ();
          clear_background Color.raywhite;
          Active.draw_active ();
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
