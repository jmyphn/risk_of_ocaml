type t
(** [t] is the representation of a contident*)

val get_name : t -> string
(** [get_name] Given a contident [c], return the name of the contident *)

val get_value : t -> int
(** [get_value] Given a contident [c], return the value of the contident. *)

val create : string -> t
(** [create] Given a string [str], creates a contident [c] with the name [str]*)

(* val owns : country list -> player -> int (** [owns] Given a list of countries
   [lst], determine if a player [t] occupies contidents and return the troop
   bonus associated with the contident(s) owned by that player *) *)
