type t

val get_name : t -> string
(** [get_name] Given a country [t], return the name of the country *)

val get_troops : t -> int
(** [get_value] Given a country [t], return its number of troops. *)

val get_player : t -> Player.t
(** [get_player] Given a country [t], return the player who owns the country. *)

val get_contident : t -> Contident.t
(** [get_contident] Given a country [t], return the contident it is in *)

val get_neighboors : t -> string list
(** [get_neighboors] Given a country [t], return its neighboors

    CAN IMPLEMENT MORE EFFICIENT DATA STRUCTURE *)

val init : Player.t list -> t list
(** [init] given a player list [lst], initialize the countries displayed on the
    board *)

val add_value : int -> t -> t
(** [add_value] Given the number of troops to be added [n] and a country
    [country], return the country with [n] added to the country. *)

val subtract_value : int -> t -> t
(** [subtract_value] Given the number of troops to be subtracted [n] and a
    country [country], return the country with [n] subtracted from the country. *)

val change_player : t -> Player.t -> int -> t
(** [change_player] Given a country [t], a player [p], and int [i], return a
    country with the given player [p] as the player and [i] as the troops*)
