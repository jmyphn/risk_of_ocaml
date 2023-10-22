type t = {
  name : string;
  troops : int;
  player : Player.t;
  contident : Contident.t;
  neighboors : string list;
}

let get_name c = c.name
let get_troops c = c.troops
let get_player c = c.player
let get_contident c = c.contident
let get_neighboors c = c.neighboors

(* Pesudo design: must incoperate randomization of players + better
   implemenetation*)
let init (p : Player.t list) : t list =
  [
    {
      name = "Maryland";
      troops = 1;
      player = List.nth p 0;
      contident = Contident.create "North America";
      neighboors = [ "New York" ];
    };
    {
      name = "New York";
      troops = 1;
      player = List.nth p 2;
      contident = Contident.create "North America";
      neighboors = [ "Maryland" ];
    };
    {
      name = "California";
      troops = 1;
      player = List.nth p 2;
      contident = Contident.create "North America";
      neighboors = [];
    };
    {
      name = "Madagascar";
      troops = 1;
      player = List.nth p 0;
      contident = Contident.create "Africa";
      neighboors = [];
    };
    {
      name = "South Africa";
      troops = 1;
      player = List.nth p 1;
      contident = Contident.create "Africa";
      neighboors = [ "Egypt" ];
    };
    {
      name = "Egypt";
      troops = 1;
      player = List.nth p 2;
      contident = Contident.create "Africa";
      neighboors = [ "South Africa" ];
    };
  ]

let add_value (n : int) (country : t) : t =
  { country with troops = country.troops + n }

let subtract_value (n : int) (country : t) : t =
  { country with troops = country.troops - n }

let change_player (country : t) (p : Player.t) (n : int) : t =
  { country with troops = n; player = p }
