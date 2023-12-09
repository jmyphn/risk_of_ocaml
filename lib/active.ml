open Raylib
open Raygui

type t = {
  bg : Texture2D.t;
  state : Texture2D.t;
  state_hl : Texture2D.t;
  phase : Texture2D.t;
  phase_hl : Texture2D.t;
}

let active = ref None
let vb = Rectangle.create 1322. 578. 200. 50.
let vb_val = ref 0
let vb_edit = ref false
let starting = ref false (* check if the phase button has been pressed *)
let x_offset = 35
let y_offset = 20
let pb_x = 1372
let pb_y = 670
let sb_x = 80
let sb_y = 650

(* state temp button hitbox *)
let state_hb =
  Rectangle.create (float_of_int sb_x) (float_of_int sb_y) 150. 100.

(* phase temp button hitbox *)
let phase_hb =
  Rectangle.create (float_of_int pb_x) (float_of_int pb_y) 150. 100.

let highlight_button_state mouse =
  if check_collision_point_rec mouse state_hb then
    match is_mouse_button_pressed MouseButton.Left with
    | false ->
        draw_texture (Option.get !active).state_hl sb_x sb_y Color.raywhite
    | true ->
        Constants.game_active := None;
        Constants.game_state := END

let highlight_button_phase mouse =
  if check_collision_point_rec mouse phase_hb then
    match is_mouse_button_pressed MouseButton.Left with
    | false ->
        draw_texture (Option.get !active).phase_hl pb_x pb_y Color.raywhite
    | true ->
        starting := true;
        Constants.game_active :=
          Some (Game.change_phase (Constants.get_game ()))

let initialize_active () =
  let active_bg = load_texture "assets/active/map.png" in
  let state = load_texture "assets/TempButton.png" in

  let state_hl = load_texture "assets/TempButtonHighlight.png" in
  let phase = load_texture "assets/active/PhaseButton.png" in

  let phase_hl = load_texture "assets/active/PhaseButtonHi.png" in
  active := Some { bg = active_bg; state; state_hl; phase; phase_hl }

let get_value_from_box () =
  (match value_box vb "" !vb_val ~min:0 ~max:100 !vb_edit with
  | vl, true ->
      vb_edit := not !vb_edit;
      vb_val := vl
  | vl, false -> vb_val := vl);
  if !vb_val = 0 then ()
  else
    (* want to use the input of vb_val w.r.t. the current phase of the game *)
    match Game.get_phase (Constants.get_game ()) with
    | Deploy -> ()
    | Attack -> ()
    | Fortify -> ()

let draw_instructions (game : Game.t) =
  if !starting = true then
    match Game.get_phase (Constants.get_game ()) with
    | Deploy ->
        draw_text
          "Current Phase is Deploy. \n\
           Choose the territory to\n\
          \ put your troops in." 1302 109 20 Color.black;
        draw_text
          ("Remaining Troops: " ^ string_of_int (Game.get_remaining_troops game))
          1322 239 20 Color.black
    | Attack -> ()
    | Fortify -> ()
  else
    draw_text "Click the change phase \nbutton to begin." 1302 108 20
      Color.black

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
            (fst location + x_offset)
            (snd location + y_offset)
            20 (Player.get_color player))
    territories

let draw_active mouse =
  let a = Option.get !active in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro a.bg source dest origin 0. Constants.default_color;
  draw_texture a.state sb_x sb_y Constants.default_color;
  draw_texture a.phase pb_x pb_y Constants.default_color;

  let game = Constants.get_game () in

  let players = Game.get_players game in
  let _ = List.iter (fun player -> draw_territories_of_player player) players in
  ();

  (* draw current player *)
  let curr_player = Game.get_current_player game in
  let curr_player_name = Player.get_name curr_player in
  let curr_player_color = Player.get_color curr_player in
  let curr_player_string = "Current Player: " ^ curr_player_name in
  draw_text curr_player_string 464 833 40 curr_player_color;
  draw_instructions game;

  (* TODO: add function that will display instructions *)

  (* change state *)
  (* highlight_button_state mouse; *)
  (* change phase *)
  highlight_button_phase mouse
