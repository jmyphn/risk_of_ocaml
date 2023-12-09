open Raylib

type game_state =
  | START
  | MENU
  | ACTIVE
  | END

type game = Game.t

(** [game_state] the current state of the game *)
let game_state = ref START

(** [game_active] the active game *)
let game_active : game option ref = ref None

(** [get_state ()] returns the current state of the game *)
let get_state () = !game_state

(** [get_game ()] returns the active game *)
let get_game () = Option.get !game_active

(** [screen_width] is the width of the screen. Default is 1400px *)
let screen_width = 1600

(** [screen_height] is the height of the screen. Default is 900px *)
let screen_height = 900

(** [screen_fps] is the fps of the screen *)
let screen_fps = 60

(** [default_color] default color *)
let default_color = Color.raywhite
