open Raylib

type game_state =
  | START
  | MENU
  | ACTIVE
  | END

(** [screen_width] is the width of the screen. Default is 1400px *)
let screen_width = 1400

(** [screen_height] is the height of the screen. Default is 900px *)
let screen_height = 900

(** [screen_fps] is the fps of the screen *)
let screen_fps = 60

(** [default_color] default color *)
let default_color = Color.raywhite
