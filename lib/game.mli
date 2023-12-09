type player = Player.t
type players = player list

type phase =
  | Deploy
  | Attack
  | Fortify  (** [phase] The phase of the current game *)

type t
(** [t] The type of the current game *)

val init : string list -> int -> t
(** [init] Given a string list [slst] of player names and int [n] representing
    the number of players, initialize the game*)

val get_players : t -> player list
(** [get_players game] Given a game [game], returns the players in this game as
    a [player list]. *)

val get_current_player : t -> player
(** [get_current_player] Given a game [g], return the current player*)

val next_player : t -> player
(** [netx_player] Given a game [g], return the next player*)

val get_phase : t -> phase
(** [get_phase] Given a game [g], return the current phase*)

val get_territories : t -> Territories.t array
(** [get_territories] Given a game [g], return the [Territories.t array]*)

val get_remaining_troops : t -> int
(** [get_remaining_troops] Given a game [g], return the remaining troops to be
    deployed. *)

val change_phase : t -> t
(** [change_phase] Given a phase [p], and a game [g], return a game with the
    phase [p]*)

val get_game_over : t -> bool
(** [get_game_over] Given a game [g], return [true] if the game is over and
    [false] otherwise *)
