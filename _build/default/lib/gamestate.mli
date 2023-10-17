type t 

val first_state : t 

val current_state : t -> string

val change_state : t -> string -> t 

val message : t -> unit
