type t
(** [t] is the representation type of the map *)

type continents = Continent.t array
(** [continents] is the representation of all the continents on the map *)

type territories = Territories.t array
(** [territories] is the representation of all territories on the map *)

val get_territories : t -> territories
(** [get_territories map] returns the territories on the map *)

val get_continents : t -> continents
(** [get_continents map] returns the continents on the map *)

val create_map : Yojson.Basic.t -> t
(** [create_map path] creates a [Map.t] instance. [path] must be a valid json
    representation of a map. *)
