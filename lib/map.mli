type t 
(** [t] is the representation type of the map *)

val create_map : t
(** [create_map] Returns the map to be displayed to players *)

val display_map : t -> unit 
(** [display_map curr_map] Given a map, prints out and displays the map to 
    players *)