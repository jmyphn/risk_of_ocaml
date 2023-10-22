type player = Player.t
type players = player list
type phase = Deploy | Attack | Fortify
type t = { players : players; current_player : player; current_phase : phase }

(* Given a int [n], initializes players and returns a list of the players
initialized. USER CHANGES NAME ECT ECT *)
let rec initialize_players (n : int) : players =
  match n with
  | 0 -> []
  | a ->
      let _ = print_endline ("Choose name for Player" ^ string_of_int a) in
      let name = read_line () in
      Player.init name :: initialize_players (n - 1)

(** Initializes game given a number of players *)
let init (numPlayers : int) : t =
  let plist = initialize_players numPlayers in
  { players = plist; current_player = List.hd plist; current_phase = Deploy }

(** Given a game, return the current player *)
let get_current_player game = game.current_player

(* Return the next player given a list of players INEFFICIENT: IMPLEMENT BETTER*)
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

(** Given a game and its phase, return a new game with the next phase. The next
    phase order: ATTACK -> FORTIFY -> DEPLOY*)
let change_phase (p : phase) (game : t) : t =
  match p with
  | Deploy ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Attack;
      }
  | Attack ->
      {
        players = game.players;
        current_player = game.current_player;
        current_phase = Fortify;
      }
  | Fortify ->
      {
        players = game.players;
        current_player = next_player game;
        current_phase = Deploy;
      }
