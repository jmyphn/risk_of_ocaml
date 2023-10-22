type t

val get_name : t -> string
(** [get_name] Given a country [t], return the name of the country *)

val get_value : t -> int
(** [get_value] Given a country [t], return the value of the country. *)

val init : string -> int -> t
(** [init] Given a country name [n] and a country value of type [troops], 
    initialize a country [t]. *)

val add_value : int -> t -> t
(** [add_value] Given the number of troops to be added [n] and a country
    [country], return the country with [n] added to the country. *)
