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

let get_player s lst = List.find (fun p -> Player.get_name p = s) lst

(* Given a int [n], initializes players and returns a list of the players
   initialized. USER CHANGES NAME ECT ECT *)
let rec init_players (n : int) : players =
  match n with
  | 0 -> []
  | a -> (
      let _ = print_endline ("Choose name for Player " ^ string_of_int a) in
      match sample colors_left (arr_size colors_left) with
      | None -> failwith "Too many players"
      | Some c -> Player.init (string_of_int n) c :: init_players (a - 1))

(**Initializes the continents in a game.*)
(* let init_continents = path |> Map.create_map |> Map.get_continents *)

(** [get_continent c] returns the continent with name c*)
(* let get_continent (c : string) : Continent.t = match Array.find_opt (fun
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

let rec roll_dice (n : int) : int list =
  match n with
  | 0 -> []
  | _ -> Random.int 6 :: roll_dice (n - 1)

let rec pp_lst pp_elt lst =
  match lst with
  | [] -> ""
  | h :: t -> pp_elt h ^ ", " ^ pp_lst pp_elt t

let attack (atk : territory) (atk_player : player) (def : territory)
    (def_player : player) =
  let _ =
    print_endline
      (Territories.get_name atk ^ " attacks " ^ Territories.get_name def)
  in
  let atk_troops = Territories.get_troops atk in
  let def_troops = Territories.get_troops def in
  let _ =
    print_endline
      ("Choose how many troops to attack with max 3 or "
      ^ string_of_int (atk_troops - 1)
      ^ ", min 1")
  in
  let num_atk_dice = int_of_string (read_line ()) in
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
    | [], [] -> print_endline "can't attack"
    | _, [] ->
        print_endline "Attack wins";
        print_endline
          ("Choose the number of troops to move over, max "
          ^ string_of_int (Territories.get_troops atk));
        let n = int_of_string (read_line ()) in
        Territories.add_value n def;
        Territories.subtract_value n atk;
        Player.add_territory atk_player def;
        Player.remove_territory def_player def
    | [], _ -> print_endline "can't attack"
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
  cmp atk_dice def_dice;
  print_endline "Attack is over"

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

(* let rec get_player_from_territory (ter : Territories.t) (plst : players) =
   match plst with | [] -> failwith "not found" | h :: t -> let t_list =
   Player.get_territories_lst h in if List.exists (fun t1 -> t1 = ter) t_list
   then h else get_player_from_territory ter t *)

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
    List.filter (fun t1 -> List.exists (fun t2 -> t1 <> t2) visited) n
  in
  let acc' = List.sort_uniq Stdlib.compare (n @ acc) in
  match new_t with
  | [] -> acc'
  | _ ->
      let tlst' = List.map (fun h -> get_territory_game h) new_t in
      fortify_territories tlst' acc' visited'

let fortify p =
  let _ = print_endline "Select a territory to move troops from: " in
  let _ = Player.territories_to_string p in
  let t1 = get_territory_game (read_line ()) in
  let _ =
    print_endline
      ("Select the number of troops to move: max "
      ^ string_of_int (Territories.get_troops t1 - 1))
  in
  let n = int_of_string (read_line ()) in
  let _ = print_endline "Choose the territory to move troops to " in
  let tlst = fortify_territories [ t1 ] [] [] in
  print_endline (pp_lst (fun s -> s) tlst);
  let t2 = get_territory_game (read_line ()) in
  Territories.add_value n t2;
  Territories.subtract_value n t1

let phase_to_string (phase : phase) : string =
  match phase with
  | Deploy -> "deploy"
  | Attack -> "attack"
  | Fortify -> "fortify"

let get_troops (_ : player) : int = Random.int 10

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

(** [Deploy_helper g] given a game [g], tell the player to deploy their troops
    in their territories and deploy those troops int the cooresponding
    territories. Only return when the player has finished deploying their
    troops. *)
let deploy_helper g =
  let new_troops = ref (get_troops g.current_player) in
  print_endline (string_of_int !new_troops);
  if !new_troops = 0 then print_endline "You have no troops to deploy"
  else
    while !new_troops > 0 do
      let _ =
        print_endline
          (string_of_int !new_troops
         ^ " troop have been drafted. Select the country you want to deploy in "
          )
      in
      let _ = print_endline (Player.territories_to_string g.current_player) in
      let input = Player.get_territory g.current_player (read_line ()) in
      let _ =
        print_endline
          ("Select the number of troops you wish to deploy: ("
         ^ string_of_int !new_troops ^ " Troops avaliable)")
      in
      let should_loop = ref true in
      let troop_input = ref 0 in
      while !should_loop do
        troop_input := int_of_string (read_line ());
        if !troop_input <= !new_troops then should_loop := false
        else should_loop := true;
        print_endline
          ("Select a troop number below " ^ string_of_int !new_troops)
      done;
      let _ = Territories.add_value !troop_input input in
      new_troops := !new_troops - !troop_input
    done;
  print_endline "You have no more troops to deploy"

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (game : t) : t =
  let _ =
    print_endline
      ("It is Player " ^ Player.get_name game.current_player ^ "'s turn")
  in
  let _ =
    print_endline ("The current phase is " ^ phase_to_string game.current_phase)
  in
  match game.current_phase with
  | Deploy ->
      deploy_helper game;
      change_phase Attack game
  | Attack ->
      let _ = print_endline "Choose which territory to use for attack." in
      let atk_opt = can_attack_territories game.current_player in
      let _ =
        print_endline (pp_lst (fun t -> Territories.get_name t) atk_opt)
      in
      let atk_ter = Player.get_territory game.current_player (read_line ()) in
      assert (List.exists (fun t -> t == atk_ter) atk_opt);
      let _ = print_endline "Choose which territory to attack." in
      let def_options = territories_to_attack atk_ter in
      let _ = print_endline (pp_lst (fun s -> s) def_options) in
      let def_input = read_line () in
      assert (List.exists (fun s -> s = def_input) def_options);
      let def_ter = get_territory_game def_input in
      attack atk_ter game.current_player def_ter
        (get_player (Territories.get_owner def_ter) game.players);
      change_phase Fortify game
  | Fortify ->
      fortify game.current_player;
      print_endline
        ("Player" ^ Player.get_name game.current_player ^ "'s turn is over.");
      game.current_player <- next_player game;
      change_phase Deploy game
