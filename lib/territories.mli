type t

val get_name : t -> string
(** [get_name] Given a territory [t], return the name of the territory *)

val get_troops : t -> int
(** [get_value] Given a territory [t], return its number of troops. *)

val get_continent : t -> Continent.t
(** [get_continent] Given a territory [t], return the continent it is in *)

val get_location : t -> int * int
(** [get_location] Given a territory [t], return x y coordinates of its position
    for Raylib GUI *)

val get_neighbours : t -> t list
(** [get_neighbours] Given a territory [t], return its neighboors

    CAN IMPLEMENT MORE EFFICIENT DATA STRUCTURE *)

val init : string -> Continent.t -> t
(** [init n c] initializes a territory with values name = n, continent = c,
    neighbours default to empty list [], troops default to 0, location defaults
    to 0 *)

val init_neighbors : t -> t list -> unit
(** [init_neighbors c nbrs] sets the neighbors of territory [c] to the list of
    neighboring countries [nbrs] *)

val add_value : int -> t -> unit
(** [add_value] Given the number of troops to be added [n] and a territory
    [territory], increase the number of troops in [territory] by [n]. *)

val subtract_value : int -> t -> unit
(** [subtract_value] Given the number of troops to be subtracted [n] and a
    territory [territory], decrease the number of troops in [territory] by [n]. *)
