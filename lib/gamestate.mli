(** [t] *)
type t 
(** Representation type of the gamestate.*)

val first_state : t 
(** [first_state] The initial state of the game. *)

val current_state : t -> string
(** [current_state state] Given a state of the game [state], return its string
    equivalent.*)

val change_state : t -> string -> t 
(** [change_state state s] Given a state [state] and the string equivalent of 
  another state [s], return the state equivalent of [s] if it follows 
  subsequently from [state]*)

val message : t -> unit
(** [message state] Given a state [state], return the desired string to be shown 
  to the player in the terminal.*)
