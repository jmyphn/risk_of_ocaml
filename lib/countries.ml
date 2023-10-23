type t = {
  name : string;
  troops : int;
  player : Player.t;
  continent : Continent.t;
  neighbours : string list;
}

let get_name c = c.name
let get_troops c = c.troops
let get_player c = c.player
let get_continent c = c.continent
let get_neighbours c = c.neighbours

(* Pesudo design: must incoperate randomization of players + better
   implemenetation*)
let init (p : Player.t list) : t list =
  let na = Continent.create "North America" 5 in
  let af = Continent.create "Africa" 4 in
  [
    {
      name = "Maryland";
      troops = 1;
      player = List.nth p 0;
      continent = na;
      neighbours = [ "New York" ];
    };
    {
      name = "New York";
      troops = 1;
      player = List.nth p 2;
      continent = na;
      neighbours = [ "Maryland" ];
    };
    {
      name = "California";
      troops = 1;
      player = List.nth p 2;
      continent = na;
      neighbours = [];
    };
    {
      name = "Madagascar";
      troops = 1;
      player = List.nth p 0;
      continent = af;
      neighbours = [];
    };
    {
      name = "South Africa";
      troops = 1;
      player = List.nth p 1;
      continent = af;
      neighbours = [ "Egypt" ];
    };
    {
      name = "Egypt";
      troops = 1;
      player = List.nth p 2;
      continent = af;
      neighbours = [ "South Africa" ];
    };
  ]

let add_value (n : int) (country : t) : t =
  { country with troops = country.troops + n }

let subtract_value (n : int) (country : t) : t =
  { country with troops = country.troops - n }

let change_player (country : t) (p : Player.t) (n : int) : t =
  { country with troops = n; player = p }
