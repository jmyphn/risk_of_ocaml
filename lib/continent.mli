type t
(** [t] is the representation of a continent*)

val get_name : t -> string
(** [get_name] Given a continent [c], return the name of the continent *)

val get_value : t -> int
(** [get_value] Given a continent [c], return the value of bonus points a player
    can obtain from owning the continent. *)

val create : Yojson.Basic.t -> t
(** [create json] Given a Yojson.Basic.t [json] reprentation of a continent,
    creates a continent [c] with name [name] and bonus value [value] *)

(* val owns : territory list -> player -> int (** [owns] Given a list of
   countries [lst], determine if a player [t] occupies continents and return the
   troop bonus associated with the continent(s) owned by that player *) *)
