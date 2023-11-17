open Player
open Game

type t =
  | START
  | MENU
  | ACTIVE
  | END

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
  print_endline ("It is Player " ^ get_name currPlayer ^ "'s turn");
  print_endline ("The current phase is " ^ phase_to_string currPhase);
  let _ =
    match currPhase with
    | Deploy ->
        let new_troops = Random.int 10 in
        let _ =
          print_endline "This is the current map";
          Map.display_map Map.create_map;
          print_endline
            (string_of_int new_troops
           ^ " have been drafted. Deploy these troops in any of the following \
              countries: ")
        in
        let _ = print_endline (Player.countries_to_string currPlayer) in
        let input = read_line () in
        if input = "finish" then (
          print_endline "Goodbye.";
          exit 0)
        else print_endline ("Deployed these troops in " ^ input ^ ".");
        print_endline "--------------------------------"
    | Attack ->
        let _ =
          Map.display_map Map.create_map;
          print_endline "Attack a country"
        in
        let input = read_line () in
        if input = "finish" then (
          print_endline "Goodbye.";
          exit 0)
        else print_endline ("Your soldiers attacked " ^ input ^ "...");
        let decision = Random.int 2 in
        if decision = 1 then
          print_endline
            "... your soldiers fought valiantly and won the country!"
        else
          print_endline
            "... your soldiers fought, but failed to capture the country. You \
             lost your country in the process!";
        print_endline "--------------------------------"
    | Fortify ->
        let _ =
          Map.display_map Map.create_map;
          print_endline "Fortify your troops"
        in
        let input = read_line () in
        if input = "finish" then (
          print_endline "Goodbye.";
          exit 0)
        else print_endline ("Fortify " ^ input ^ " troops.");
        print_endline "--------------------------------"
  in
  play (Game.change_phase currPhase game)

(**** Game Initialization functions***)

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
      let _ =
        print_endline
          "Choose between 2-4 players. ** In this version, the number of \
           players will default to 3 players. **"
      in
      let num_players = read_line () in
      if int_of_string_opt num_players = None then (
        print_endline
          "Sorry, this is not a valid player count. Defaulting to 3 players.";
        play (Game.init 3))
      else if int_of_string num_players < 2 then (
        print_endline
          "Sorry, this is not a valid player count. Defaulting to 3 players.";
        play (Game.init 3))
      else if int_of_string num_players > 4 then (
        print_endline
          "Sorry, this is not a valid player count. Defaulting to 3 players.";
        play (Game.init 3))
      else play (Game.init 3)
  | END -> ()
