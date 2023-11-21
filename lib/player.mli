type t
(** [t] Represents a player *)

val get_name : t -> string
(** [get_name p] Given a player [p] returns the name of [p]. *)

val rep_ok : t -> unit
(* [rep_ok p] Checks if [p] satisfies the representation invariant *)

val get_color : t -> Raylib.Color.t
(** [get_color p] Given a player [p] returns the color of [p]. *)

val get_countries : t -> Countries.t option array
(** [get_countries p] Given a player [p] returns the countries owned by [p]. *)

val get_countries_lst : t -> Countries.t list
(** [get_countries_lst p] Given a player [p] returns the list of countries owned
    by [p].*)

val add_country : t -> Countries.t -> unit
(** [add_country p c] Given a player [p] and country [c], add the country to the
    player [p]. *)

val remove_country : t -> Countries.t -> unit
(** [add_country p c] Given a player [p] and country [c], remove the country
    from the player [p]. Requires: [c] must be owned by the player*)

val num_countries : t -> int
(** [num_countries p] Given a player [p] returns the number of countries owned
    by [p]. *)

val countries_to_string : t -> string

val init : Raylib.Color.t -> t
(** [init c] Initializes a player given a color [Raylib.Color.t].*)
