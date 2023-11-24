(* open Yojson.Basic.Util *)

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
}

(* Holds the current game state *)
let game = ref None

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
let continents : (string * Continent.t) array =
  [|
    ("North America", Continent.create "North America" 5);
    ("South America", Continent.create "South America" 3);
    ("Africa", Continent.create "Africa" 3);
    ("Europe", Continent.create "Europe" 5);
    ("Asia", Continent.create "Asia" 7);
    ("Australia", Continent.create "Australia" 3);
  |]

(** [get_continent c] returns the continent with name c*)
let get_continent (c : string) : Continent.t =
  match Array.find_opt (fun (k, _) -> c = k) continents with
  | None -> failwith "Not a continent"
  | Some (_, v) -> v

(**Initializes the Territories in a game TODO: parse json file(s) *)
let init_territories : Territories.t array =
  [|
    Territories.init "Ontario" (get_continent "North America");
    Territories.init "Alberta" (get_continent "North America");
    Territories.init "Western US" (get_continent "North America");
    Territories.init "Eastern US" (get_continent "North America");
    Territories.init "Alaska" (get_continent "North America");
    Territories.init "Central America" (get_continent "North America");
  |]

let to_option_array (arr : 'a array) : 'a option array =
  Array.map (fun v -> Some v) arr

(** Assigns Territories to each player randomly.*)
let assign_Territories plst =
  let _ = Random.self_init () in
  let temp = to_option_array (Array.copy init_territories) in
  for i = 0 to arr_size temp - 1 do
    let _ = print_endline (string_of_int i) in
    let _ = print_endline (string_of_int (arr_size temp)) in
    let player_ind = i mod List.length plst in
    let player = List.nth plst player_ind in
    let c = sample temp (arr_size temp) in
    match c with
    | None -> failwith "impossible fdas"
    | Some c1 -> Player.add_territory player c1
  done;
  plst

(** Initializes game given a number of players *)
let init (numPlayers : int) =
  let plist = assign_Territories (init_players numPlayers) in
  game :=
    Some
      {
        players = plist;
        current_player = List.hd plist;
        current_phase = Deploy;
        territories = init_territories;
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

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (p : phase) (game : t) : t =
  match p with
  | Deploy ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Attack;
        territories = game.territories;
      }
  | Attack ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Fortify;
        territories = game.territories;
      }
  | Fortify ->
      {
        players = game.players;
        current_player = next_player game;
        current_phase = Deploy;
        territories = game.territories;
      }
