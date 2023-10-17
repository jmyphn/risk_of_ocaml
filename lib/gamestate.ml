type t = START | MENU | ACTIVE | PLAYER_ATTACK_ END

let first_state = START

(***)
let current_state (s : t) = 
  match s with
  | START -> "start"
  | MENU -> "menu"
  | ACTIVE -> "active"
  | END -> "end"

(***)
let change_state s1 input = 
  match s1, input with 
    | START, "continue" -> MENU
    | MENU, "play" -> ACTIVE 
    | ACTIVE, "finish" -> END
    | ACTIVE, "attack" -> 4
    | END, "restart" -> MENU
    | state,_ -> let _ = print_endline("Invalid command. Try again.") in 
                  state

(***)
let message s = match s with
  | START -> print_endline ("Welcome to Risk! Type 'continue' to go to the Menu.")
  | MENU -> print_endline ("This is the Menu Page. Type 'play' to start the game.")
  | ACTIVE -> print_endline ("Type 'finish' to end the game.")
  | END -> print_endline ("Type 'restart' to go back to the menu.")
  
  
