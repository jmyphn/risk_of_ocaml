type player = Player.t
type players = player list
type country = Countries.t
type countries = country array

type phase =
  | Deploy
  | Attack
  | Fortify

type t = {
  players : players;
  current_player : player;
  current_phase : phase;
  countries : countries;
}

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

(**Returns the number of available colors in colors_left*)
let color_size =
  let count = ref 0 in
  for i = 0 to Array.length colors_left - 1 do
    if colors_left.(i) = None then count := i
  done;
  !count

(** Returns a new array without None values nested between Some values.*)
(* let shift_array arr = let new_list = Array.fold_left (fun acc v -> if v =
   None then acc else v :: acc) [] arr in Array.of_list new_list *)

(**Returns a random color from colors_left. *)
let sample_color =
  let i = Random.int color_size in
  let color = colors_left.(i) in
  colors_left.(i) <- colors_left.(color_size);
  colors_left.(color_size) <- None;
  color

(* Given a int [n], initializes players and returns a list of the players
   initialized. USER CHANGES NAME ECT ECT *)
let rec init_players (n : int) : players =
  match n with
  | 0 -> []
  | a -> (
      let _ = print_endline ("Choose name for Player" ^ string_of_int a) in
      match sample_color with
      | None -> failwith "Too many players"
      | Some c -> Player.init c :: init_players (n - 1))

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

(**Initializes the countries in a game*)
let init_countries : Countries.t array =
  [|
    Countries.init "Ontario" (get_continent "North America") [];
    Countries.init "Alberta" (get_continent "North America") [];
    Countries.init "Western US" (get_continent "North America") [];
    Countries.init "Eastern US" (get_continent "North America") [];
    Countries.init "Alaska" (get_continent "North America") [];
    Countries.init "Central America" (get_continent "North America") [];
  |]

let sample arr size =
  let i = Random.int size in
  let v = arr.(i) in
  arr.(i) <- arr.(size);
  arr.(size) <- None;
  v

let arr_size arr =
  let count = ref 0 in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) = None then count := i
  done;
  !count

let to_option_array (arr : 'a array) : 'a option array =
  Array.map (fun v -> Some v) arr

(** Assigns all the countries to the first player*)
let assign_countries plst =
  let temp = to_option_array (Array.copy init_countries) in
  while arr_size temp > 0 do
    let c = sample temp (arr_size temp) in
    match c with
    | None -> failwith "impossible"
    | Some c1 -> Player.add_country (List.hd plst) c1
  done;
  plst

(** Initializes game given a number of players *)
let init (numPlayers : int) : t =
  let plist = assign_countries (init_players numPlayers) in
  {
    players = plist;
    current_player = List.hd plist;
    current_phase = Deploy;
    countries = init_countries;
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

(** Given a game, return the countries*)
let get_countries game = game.countries

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (p : phase) (game : t) : t =
  match p with
  | Deploy ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Attack;
        countries = game.countries;
      }
  | Attack ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Fortify;
        countries = game.countries;
      }
  | Fortify ->
      {
        players = game.players;
        current_player = next_player game;
        current_phase = Deploy;
        countries = game.countries;
      }
