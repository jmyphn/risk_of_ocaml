type t
(** Represents a player type with name and countries attribute.*)

val num_countries : t -> int
(** Returns the number of countries a player owns.*)

val get_name : t -> string
(** Returns the name of a player.*)

val init : string -> t
(** Initializes a player given a string [name].*)
