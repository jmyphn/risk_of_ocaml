type t
(** [t] is the representation of a continent*)

val get_name : t -> string
(** [get_name] Given a continent [c], return the name of the continent *)

val get_value : t -> int
(** [get_value] Given a continent [c], return the value of bonus points a player
    can obtain from owning the continent. *)

val create : string -> int -> t
(** [create] Given a string [str], creates a continent [c] with the name [str]*)

(* val owns : territory list -> player -> int (** [owns] Given a list of
   countries [lst], determine if a player [t] occupies continents and return the
   troop bonus associated with the continent(s) owned by that player *) *)
