type t = { name : string; countries : Countries.t list }
(** player representation type using a record*)

(** Given player [p], returns the number of countries it owns.*)
let num_countries (p : t) : int = List.length p.countries

(** Given a player [p], returns the name of the player. *)
let get_name (p : t) : string = p.name

(** Given a name [n], initializes the player.*)
let init (n : string) : t = { name = n; countries = [] }
