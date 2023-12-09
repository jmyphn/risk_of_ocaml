open Raylib
open Raygui

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
let two_pb_hb = Rectangle.create 304. 348. 100. 100.
let three_pb_hb = Rectangle.create 527. 348. 100. 100.
let four_pb_hb = Rectangle.create 750. 348. 100. 100.
let five_pb_hb = Rectangle.create 971. 348. 100. 100.
let six_pb_hb = Rectangle.create 1194. 348. 100. 100.
let players = ref ([], 7) (* player list, number of players *)
let tb_edit = ref false
let tb_text = ref "Enter Player"
let tb = Rectangle.create 550. 600. 500. 80.
let show_tb = ref false

(* TODO: REMOVE *)
let rec pp_lst pp_elt lst =
  match lst with
  | [ h ] -> pp_elt h
  | h :: t -> pp_elt h ^ ", " ^ pp_lst pp_elt t
  | [] -> ""

let grab_text_in_box () =
  if !tb_text = "" then ()
  else
    match is_key_pressed Enter with
    | true ->
        if List.mem !tb_text (fst !players) then (
          print_endline (!tb_text ^ " already inside");
          print_endline (pp_lst (fun s -> s) (fst !players)))
        else (
          print_endline (!tb_text ^ " added");
          players := (!tb_text :: fst !players, snd !players);
          print_endline (pp_lst (fun s -> s) (fst !players)));

        tb_text := ""
    | _ -> ()

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
    match is_mouse_button_pressed MouseButton.Left with
    | false -> draw_texture highlight x y Color.raywhite
    | true ->
        Constants.game_active := Some (Game.init n);
        Constants.game_state := INSTRUCTIONS;
        show_tb := true;
        players := (fst !players, n) 
(* Constants.game_active := Some (Game.init n) *)
(* Constants.game_state := ACTIVE *)

let draw_menu mouse =
  let menu = Option.get !menu in
  let sw = float_of_int Constants.screen_width in
  let sh = float_of_int Constants.screen_height in
  let source = Rectangle.create 0. 0. sw sh in
  let dest = Rectangle.create 0. 0. sw sh in
  let origin = Vector2.create 0. 0. in
  draw_texture_pro menu.bg source dest origin 0. Constants.default_color;
  if not !show_tb then (
    draw_texture menu.two_pb 304 348 Constants.default_color;
    draw_texture menu.three_pb 527 348 Constants.default_color;
    draw_texture menu.four_pb 750 348 Constants.default_color;
    draw_texture menu.five_pb 971 348 Constants.default_color;
    draw_texture menu.six_pb 1194 348 Constants.default_color;
    highlight_button_menu mouse two_pb_hb menu.two_pb_hl (294, 343) 2;
    (* 2 *)
    highlight_button_menu mouse three_pb_hb menu.three_pb_hl (517, 343) 3;
    (* 3 *)
    highlight_button_menu mouse four_pb_hb menu.four_pb_hl (740, 343) 4;
    (* 4 *)
    highlight_button_menu mouse five_pb_hb menu.five_pb_hl (961, 343) 5;
    (* 5 *)
    highlight_button_menu mouse six_pb_hb menu.six_pb_hl (1184, 343) 6
    (* 6 *))
  else grab_text_in_box ();
  (* rect: shape and position of the text box on screen *)
  (let rect = Rectangle.create 25.0 215.0 125.0 30.0 in
   if !show_tb then
     match text_box rect !tb_text !tb_edit with
     (* vl is the text inside the textbox *)
     | vl, true ->
         tb_edit := not !tb_edit;
         tb_text := vl
     | vl, false ->
         tb_text := vl;

         set_style (TextBox `Text_alignment) TextAlignment.(to_int Left));
  let plst = fst !players in
  let np = snd !players in
  if List.length plst = np then (
    Constants.game_active := Some (Game.init plst np);
    Constants.game_state := ACTIVE)
  else ()
