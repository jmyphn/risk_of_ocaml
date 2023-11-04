type t

val get_name : t -> string
(** [get_name] Given a country [t], return the name of the country *)

val get_troops : t -> int
(** [get_value] Given a country [t], return its number of troops. *)

val get_continent : t -> Continent.t
(** [get_continent] Given a country [t], return the continent it is in *)

val get_location : t -> int * int
(** [get_location] Given a country [t], return x y coordinates of its position
    for Raylib GUI *)

val get_neighbours : t -> string list
(** [get_neighbours] Given a country [t], return its neighboors

    CAN IMPLEMENT MORE EFFICIENT DATA STRUCTURE *)

val init : unit -> t list
(** [init] given a player list [lst], initialize the countries displayed on the
    board *)

val add_value : int -> t -> t
(** [add_value] Given the number of troops to be added [n] and a country
    [country], return the country with [n] added to the country. *)

val subtract_value : int -> t -> t
(** [subtract_value] Given the number of troops to be subtracted [n] and a
    country [country], return the country with [n] subtracted from the country. *)
