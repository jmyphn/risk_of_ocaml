type t

val get_name : t -> string
(** [get_name] Given a territory [t], return the name of the territory *)

val get_troops : t -> int
(** [get_value] Given a territory [t], return its number of troops. *)

val get_continent : t -> string
(** [get_continent] Given a territory [t], return the continent it is in *)

val get_location : t -> string
(** [get_location] Given a territory [t], return the extension tag of the
    territory *)

val get_neighbours : t -> string list
(** [get_neighbours] Given a territory [t], return its neighboors

    CAN IMPLEMENT MORE EFFICIENT DATA STRUCTURE *)

val init : Yojson.Basic.t -> t
(** [init n c] initializes a territory with values name = n, continent = c,
    neighbours default to empty list [], troops default to 0, location defaults
    to 0 *)

val add_value : int -> t -> unit
(** [add_value] Given the number of troops to be added [n] and a territory
    [territory], increase the number of troops in [territory] by [n]. *)

val subtract_value : int -> t -> unit
(** [subtract_value] Given the number of troops to be subtracted [n] and a
    territory [territory], decrease the number of troops in [territory] by [n]. *)