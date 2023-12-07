module Y = Yojson.Basic.Util

type player = Player.t
type players = player list
type territory = Territories.t
type territories = territory array

type phase =
  | Deploy
  | Attack
  | Fortify

type t = {
  players : players;
  mutable current_player : player;
  current_phase : phase;
  territories : territories;
  troops_to_place : int;
}

(* Holds the current map *)
let json = "data/countries.json"
let path = Yojson.Basic.from_file json

(**Array of colors that have not been taken by a player. If None, then the color
   has been taken. RI: All Some values are to the left of the array*)
let colors_left : Raylib.Color.t option array =
  [|
    Some Raylib.Color.gray;
    Some Raylib.Color.blue;
    Some Raylib.Color.green;
    Some Raylib.Color.red;
    Some Raylib.Color.yellow;
    Some Raylib.Color.purple;
  |]

(*************************** Helpers **********************************)
let rec pp_lst pp_elt lst =
  match lst with
  | [] -> ""
  | h :: t -> pp_elt h ^ ", " ^ pp_lst pp_elt t

let pp_territory_list t = pp_lst (fun s -> s) t
let get_player s lst = List.find (fun p -> Player.get_name p = s) lst

(** [Catch_error f msg] Runs the function f which takes in an input
    (readline()), and reruns the function and prints a message if there is and
    error. *)
let rec catch_error f msg =
  try f (read_line ())
  with _ ->
    print_endline msg;
    catch_error f msg

(***************************Sampling helpers**********************************)

(**Returns the number of non-None elements in an option array that has shifted
   all None elements right. Requires: All Some values are left of all None
   values. *)
let arr_size arr =
  if arr.(Array.length arr - 1) <> None then Array.length arr
  else
    let counter = ref 0 in
    while arr.(!counter) <> None && !counter < Array.length arr - 1 do
      incr counter
    done;
    !counter

(** Samples from an option array and removes that element from the array by
    switching it to option. Requires: All Some values are left of all None
    values. *)
let sample arr size =
  if size <= 0 then None
  else
    let i = Random.int size in
    let v = arr.(i) in
    (* swap *)
    arr.(i) <- arr.(size - 1);
    arr.(size - 1) <- None;
    v
(*************************************************************************)

(*************************************************************************)

(*************************** Initialization**********************************)
(* Given a int [n], initializes players and returns a list of the players
   initialized. USER CHANGES NAME ECT ECT *)
let rec init_players (n : int) : players =
  match n with
  | 0 -> []
  | a -> (
      print_endline ("\nSelect name for player " ^ string_of_int a);
      let input = read_line () in
      print_endline ("Player " ^ string_of_int a ^ " Succesfully Initialized");
      match sample colors_left (arr_size colors_left) with
      | None -> failwith "Too many players"
      | Some c -> Player.init input c :: init_players (a - 1))

(**Initializes the continents in a game.*)
(* let init_continents = path |> Map.create_map |> Map.get_continents *)

(** [get_continent c] returns the continent with name c*)
(*= let get_continent (c : string) : Continent.t = match Array.find_opt (fun
   continent -> Continent.get_name continent = c) continents with | None ->
   failwith "Not a continent" | Some c -> c *)

(**Inihtializes the Territories in a game *)
let init_territories = path |> Map.create_map |> Map.get_territories

let get_territory_game (s : string) : Territories.t =
  let ter =
    Array.find_opt (fun t -> Territories.get_name t = s) init_territories
  in
  match ter with
  | None -> failwith "impossible"
  | Some c -> c

let check_territory_rep g =
  let t =
    List.fold_left
      (fun acc (p : player) -> Player.get_territories_lst p @ acc)
      [] g.players
  in
  List.length (List.sort_uniq Stdlib.compare t) = 42

(** Checks representation invariants during gameplay.*)
let rep_ok g = if check_territory_rep g then g else failwith "rep-inv violated"

let to_option_array (arr : 'a array) : 'a option array =
  Array.map (fun v -> Some v) arr

(** Assigns Territories to each player randomly.*)
let assign_Territories plst =
  let _ = Random.self_init () in
  let temp = to_option_array (Array.copy init_territories) in
  for i = 0 to arr_size temp - 1 do
    let player_ind = i mod List.length plst in
    let player = List.nth plst player_ind in
    let c = sample temp (arr_size temp) in
    match c with
    | None -> failwith "impossible"
    | Some c1 -> Player.add_territory player c1
  done;
  plst

let num_troops_per_player n =
  match n with
  | 2 -> 40
  | 3 -> 35
  | 4 -> 30
  | 5 -> 25
  | 6 -> 20
  | _ -> failwith "Invalid number of players"

let assign_troops_helper t p =
  let countries = Player.get_territories p in
  for i = 0 to t - 1 do
    let index = i mod arr_size countries in
    match countries.(index) with
    | None -> failwith "impossible"
    | Some c -> Territories.add_value 1 c
  done;
  p

let assign_troops n plst = List.map (assign_troops_helper n) plst

(** Initializes game given a number of players *)
let init (numPlayers : int) =
  let plist =
    assign_troops
      (num_troops_per_player numPlayers)
      (assign_Territories (init_players numPlayers))
  in
  {
    players = plist;
    current_player = List.hd plist;
    current_phase = Deploy;
    territories = init_territories;
    troops_to_place = 0;
  }

(****************************************************************************)

(******************************* ATTACK****************************************)

let rec roll_dice (n : int) : int list =
  match n with
  | 0 -> []
  | _ -> Random.int 6 :: roll_dice (n - 1)

(**Given a player, returns a list of the territories that can attack (have more
   than one troop)*)
let can_attack_territories p =
  let t_lst = Player.get_territories_lst p in
  List.filter (fun t -> Territories.get_troops t > 1) t_lst

let territories_to_attack t =
  let neighbours = Territories.get_neighbours t in
  List.filter
    (fun n ->
      let t2 = get_territory_game n in
      Territories.get_owner t2 <> Territories.get_owner t)
    neighbours

let rec attack (game : t) =
  print_endline "\n\n\nDo you want to attack? (Yes/No).";
  let err =
    catch_error
      (fun x ->
        match x with
        | "yes" -> x
        | "no" -> x
        | "Yes" -> x
        | "No" -> x
        | _ -> failwith "")
      "Invalid Input"
  in
  if err = "no" || err = "No" then ()
  else print_endline "Choose which territory to attack with:";
  let atk_opt = can_attack_territories game.current_player in
  print_endline (Player.usable_territories_to_string game.current_player);
  let atk_ter =
    catch_error
      (fun x ->
        if
          List.exists
            (fun t -> t = Player.get_territory game.current_player x)
            atk_opt
        then Player.get_territory game.current_player x
        else failwith "")
      "Invalid input"
  in
  let atk_player = get_player (Territories.get_owner atk_ter) game.players in
  print_endline "\nChoose which territory to attack:";
  let def_options = territories_to_attack atk_ter in
  print_endline (pp_territory_list def_options);
  let def_ter =
    catch_error
      (fun x ->
        if List.exists (fun s -> s = x) def_options then get_territory_game x
        else failwith "")
      "Invalid input"
  in
  let def_player = get_player (Territories.get_owner def_ter) game.players in
  attacking atk_ter atk_player def_ter def_player game

and attacking atk atk_player def def_player game =
  print_endline
    (Territories.get_name atk ^ " attacks " ^ Territories.get_name def);

  let atk_troops = Territories.get_troops atk in
  let def_troops = Territories.get_troops def in
  let _ =
    print_endline
      ("Choose how many troops to attack with max 3 or "
      ^ string_of_int (atk_troops - 1)
      ^ ", min 1")
  in
  let num_atk_dice = catch_error int_of_string "Invalid Input" in
  let atk_dice =
    match num_atk_dice with
    | n when n > 2 -> roll_dice 3
    | n when n > 0 -> roll_dice n
    | _ -> failwith "violates rep_inv"
  in
  let _ = print_endline ("attack rolls: " ^ pp_lst string_of_int atk_dice) in
  let def_dice =
    match def_troops with
    | n when n > 1 -> roll_dice 2
    | n when n > 0 -> roll_dice n
    | _ -> failwith "violates\n   rep_inv"
  in
  let _ = print_endline ("defense rolls: " ^ pp_lst string_of_int def_dice) in
  let rec cmp (a : int list) (d : int list) : unit =
    match
      ( List.rev (List.sort Stdlib.compare a),
        List.rev (List.sort Stdlib.compare d) )
    with
    | [], [] -> attack game
    | _, [] ->
        let d_t = Territories.get_troops def in
        if d_t <= 0 then (
          print_endline "Attack wins";
          print_endline
            ("Choose the number of troops to move over, max "
            ^ string_of_int (Territories.get_troops atk - 1));
          let n = catch_error int_of_string "Invalid Input" in
          Territories.add_value n def;
          Territories.subtract_value n atk;
          Player.add_territory atk_player def;
          Player.remove_territory def_player def;
          attack game)
        else attack game
    | [], _ ->
        print_endline "Attack lost";
        attack game
    | h :: t, h2 :: t2 ->
        if h > h2 then (
          let _ = print_endline "Defense lost one troop" in
          Territories.subtract_value 1 def;
          cmp t t2)
        else
          let _ = print_endline "Attack lost one troop" in
          Territories.subtract_value 1 atk;
          cmp t t2
  in
  cmp atk_dice def_dice

(****************************************************************************)

(******************************Fortify***********************************)
let owned_neighbours t =
  let neighbours = Territories.get_neighbours t in
  List.filter
    (fun n ->
      let t2 = get_territory_game n in
      Territories.get_owner t2 = Territories.get_owner t)
    neighbours

let rec fortify_territories tlst acc (visited : string list) =
  let t = List.hd tlst in
  let visited' = Territories.get_name t :: visited in
  let n = owned_neighbours t in
  let new_t =
    List.filter (fun t1 -> List.exists (fun t2 -> t1 = t2) visited = false) n
  in
  let acc' = List.sort_uniq Stdlib.compare (new_t @ acc) in
  match new_t with
  | [] -> acc'
  | _ ->
      let tlst' = List.map (fun h -> get_territory_game h) new_t in
      fortify_territories tlst' acc' visited'

let fortify p =
  let _ = print_endline "Select a territory to move troops from: " in
  let _ = print_endline (Player.territories_to_string p) in
  let t1 = catch_error get_territory_game "Invalid Input" in
  let _ =
    print_endline
      ("Select the number of troops to move: max "
      ^ string_of_int (Territories.get_troops t1 - 1))
  in
  let n = catch_error int_of_string "Invalid Input" in
  let _ = print_endline "Choose the territory to move troops to " in
  let tlst = fortify_territories [ t1 ] [] [] in
  print_endline (pp_lst (fun s -> s) tlst);
  let t2 = catch_error get_territory_game "Invalid Input" in
  Territories.add_value n t2;
  Territories.subtract_value n t1

(****************************************************************************)

(** [get_troops p] given a player [p], return the amount of troops they are able
    to deploy*)
let get_troops (p : player) : int =
  let n =
    int_of_float (ceil (float_of_int (Player.num_territories p) /. 3.))
    + Player.get_continent_bonus p
  in
  if n < 3 then 3 else n

(** [Deploy_helper g] given a game [g], tell the player to deploy their troops
    in their territories and deploy those troops int the cooresponding
    territories. Only return when the player has finished deploying their
    troops. *)
let deploy_helper g =
  let new_troops = ref (get_troops g.current_player) in
  if !new_troops = 0 then
    failwith "IMPOSSIBLE: Each player must have 3 troops minimum"
  else
    while !new_troops > 0 do
      print_endline
        ("\n" ^ string_of_int !new_troops
       ^ " troops have been drafted. Select the country you want to deploy in: "
        );
      let _ = print_endline (Player.territories_to_string g.current_player) in
      let input =
        catch_error
          (Player.get_territory g.current_player)
          "Invalid Territory Name"
      in
      let should_loop = ref true in
      let troops_chosen = ref 0 in
      while !should_loop do
        print_endline
          ("Select the number of troops you wish to deploy: ("
         ^ string_of_int !new_troops ^ " Troops avaliable)");
        troops_chosen :=
          catch_error
            (fun x ->
              if int_of_string x <= !new_troops then int_of_string x
              else failwith "")
            "Invalid Input";
        if !troops_chosen <= !new_troops then should_loop := false
        else should_loop := true
      done;
      let _ = Territories.add_value !troops_chosen input in
      new_troops := !new_troops - !troops_chosen
    done;
  print_endline "You have no more troops to deploy"

(**********************Phase change helpers************************************)
let phase_to_string (phase : phase) : string =
  match phase with
  | Deploy -> "deploy"
  | Attack -> "attack"
  | Fortify -> "fortify"

(** [Change_phase p g] given phase [p] and a game [g] return the game with the
    phase changed to phase [p]*)
let change_phase p g =
  rep_ok
    {
      players = g.players;
      current_player = g.current_player;
      current_phase = p;
      territories = g.territories;
      troops_to_place = g.troops_to_place;
    }

(** Given a game, return the current player *)
let get_current_player game = game.current_player

(* Return the next player given a list of players INEFFICIENT: IMPLEMENT
   BETTER*)
let rec next_player_helper plist cp original =
  match plist with
  | [] -> failwith ""
  | [ _ ] -> List.hd original
  | h1 :: h2 :: t ->
      if h1 = cp then h2 else next_player_helper (h2 :: t) cp original

(** given a game, return the next player*)
let next_player game =
  next_player_helper game.players game.current_player game.players

(** Given a game, return the phase*)
let get_phase game = game.current_phase

(** Given a game, return the Territories*)
let get_territories game = game.territories
(**********************************************************)

(********************** Phase Change **************************************)

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (game : t) : t =
  print_endline
    ("\n\n\n\n\n\n\n\n\n\nIt is "
    ^ Player.get_name game.current_player
    ^ "'s turn");
  print_endline ("The current phase is " ^ phase_to_string game.current_phase);
  match game.current_phase with
  | Deploy ->
      deploy_helper game;
      change_phase Attack game
  | Attack ->
      attack game;
      change_phase Fortify game
  | Fortify ->
      fortify game.current_player;
      print_endline
        ("Player" ^ Player.get_name game.current_player ^ "'s turn is over.");
      game.current_player <- next_player game;
      change_phase Deploy game
