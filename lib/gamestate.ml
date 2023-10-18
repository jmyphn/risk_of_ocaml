type t = START | MENU | ACTIVE | END

let first_state = START

(***Gameplay functions*)
let phase_to_string (phase : Game.phase) : string =
  match phase with
  | Deploy -> "deploy"
  | Attack -> "attack"
  | Fortify -> "fortify"

let rec play game =
  let currPlayer = Game.get_current_player game in
  let currPhase = Game.get_phase game in
  print_endline ("It is Player " ^ Player.get_name currPlayer ^ "'s turn");
  print_endline ("The current phase is " ^ phase_to_string currPhase);
  let _ =
    match currPhase with
    | Deploy ->
        let _ = print_endline "Deploy your troops" in
        let input = read_line () in
        print_endline ("Deployed " ^ input ^ " troops.")
    | Attack ->
        let _ = print_endline "Attack" in
        let input = read_line () in
        print_endline ("Attack " ^ input ^ " territory.")
    | Fortify ->
        let _ = print_endline "Fortify your troops" in
        let input = read_line () in
        print_endline ("Fortify " ^ input ^ " troops.")
  in
  play (Game.change_phase currPhase game)

(**** Game Initialization functions***)
(* let init_players np = failwith "unimplemented"

   let assign_countries c players = failwith "unimplemented"

   let num_troops players = failwith "unimplemented"

   let assign_troops countries player = failwith "unimplemented" *)

(***)
let current_state (s : t) =
  match s with
  | START -> "start"
  | MENU -> "menu"
  | ACTIVE -> "active"
  | END -> "end"

(***)
let change_state s1 input =
  match (s1, input) with
  | START, "continue" -> MENU
  | MENU, "play" -> ACTIVE
  | ACTIVE, "finish" -> END
  | END, "restart" -> MENU
  | state, _ ->
      let _ = print_endline "Invalid command. Try again." in
      state

(***)
let message s =
  match s with
  | START -> print_endline "Welcome to Risk! Type 'continue' to go to the Menu."
  | MENU ->
      print_endline "This is the Menu Page. Type 'play' to start the game."
  | ACTIVE -> print_endline "Type 'finish' to end the game."
  | END -> print_endline "Type 'restart' to go back to the menu."

let action s =
  match s with
  | START -> ()
  | MENU -> ()
  | ACTIVE ->
      let _ = print_endline "How many players?" in
      let num_players = read_line () in
      play (Game.init (int_of_string num_players))
  | END -> ()
