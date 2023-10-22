type player = Player.t
type players = player list

type phase =
  | Deploy
  | Attack
  | Fortify

type t
(** [t] The type of the game*)

val init : int -> t
(** [init] Given an int [n] representing the number of players, initialize the
    game*)

val get_current_player : t -> player
(** [get_current_player] Given a game [g], return the current player*)

val next_player : t -> player
(** [netx_player] Given a game [g], return the next player*)

val get_phase : t -> phase
(** [get_phase] Given a game [g], return the current phase*)

val get_countries : t -> Countries.t list
(** [get_phase] Given a game [g], return the country list*)

val change_phase : phase -> t -> t
(** [change_phase] Given a phase [p], and a game [g], return a game with the
    phase [p]*)

val countries_owned : Countries.t list -> player -> Countries.t list
(** [countries_owned] Given a game [g] and a player [p], return all the
    countries owned by the player [p]*)

val country_to_string : Countries.t list -> string
(** converts a country to string*)

(* val deploy : t -> t val attack : t -> t val fortify : t -> t *)
