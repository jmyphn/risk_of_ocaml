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
  current_player : player;
  current_phase : phase;
  territories : territories;
  troops_to_place : int;
}

(* Holds the current game state *)
let game : t option ref = ref None

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

(* Given a int [n], initializes players and returns a list of the players
   initialized. USER CHANGES NAME ECT ECT *)
let rec init_players (n : int) : players =
  match n with
  | 0 -> []
  | a -> (
      let _ = print_endline ("Choose name for Player" ^ string_of_int a) in
      match sample colors_left (arr_size colors_left) with
      | None -> failwith "Too many players"
      | Some c -> Player.init c :: init_players (a - 1))

(**Initializes the continents in a game.*)
(* let init_continents = path |> Map.create_map |> Map.get_continents *)

(** [get_continent c] returns the continent with name c*)
(* let get_continent (c : string) : Continent.t = match Array.find_opt (fun
   continent -> Continent.get_name continent = c) continents with | None ->
   failwith "Not a continent" | Some c -> c *)

(**Initializes the Territories in a game *)
let init_territories = path |> Map.create_map |> Map.get_territories

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

(* let rec roll_dice (n : int) : int list = match n with | 0 -> [] | _ ->
   Random.int 6 :: roll_dice (n - 1)

   let attack (atk : territory) (atk_player : player) (def : territory)
   (def_player : player) = let atk_troops = Territories.get_troops atk in let
   def_troops = Territories.get_troops def in let atk_dice = match atk_troops
   with | n when n > 2 -> roll_dice 3 | n when n > 0 -> roll_dice n | _ ->
   failwith "violates rep_inv" in let def_dice = match def_troops with | n when
   n > 1 -> roll_dice 2 | n when n > 0 -> roll_dice n | _ -> failwith "violates
   rep_inv" in let rec cmp (a : int list) (d : int list) : unit = match (
   List.rev (List.sort Stdlib.compare a), List.rev (List.sort Stdlib.compare d)
   ) with | [], [] -> () | _, [] -> Player.add_territory atk_player def;
   Player.remove_territory def_player def | [], _ -> failwith "can't attack" | h
   :: t, h2 :: t2 -> if h > h2 then Territories.subtract_value 1 def else
   Territories.subtract_value 1 atk; cmp t t2 in cmp atk_dice def_dice *)

(** Initializes game given a number of players *)
let init (numPlayers : int) =
  let plist =
    assign_troops
      (num_troops_per_player numPlayers)
      (assign_Territories (init_players numPlayers))
  in
  game :=
    Some
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

let phase_to_string (phase : phase) : string =
  match phase with
  | Deploy -> "deploy"
  | Attack -> "attack"
  | Fortify -> "fortify"

let get_troops (_ : player) : int = Random.int 10

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (p : phase) (game : t) : t =
  let _ =
    print_endline
      ("It is Player " ^ Player.get_name game.current_player ^ "'s turn")
  in
  let _ = print_endline ("The current phase is " ^ phase_to_string p) in
  match p with
  | Deploy ->
      let new_troops = get_troops game.current_player in
      let _ =
        print_endline
          (string_of_int new_troops
         ^ " have been drafted. Deploy these troops in any of the following \
            countries: ")
      in
      let _ =
        print_endline (Player.territories_to_string game.current_player)
      in
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Attack;
        territories = game.territories;
        troops_to_place = game.troops_to_place;
      }
  | Attack ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Fortify;
        territories = game.territories;
        troops_to_place = game.troops_to_place;
      }
  | Fortify ->
      {
        players = game.players;
        current_player = next_player game;
        current_phase = Deploy;
        territories = game.territories;
        troops_to_place = game.troops_to_place;
      }
