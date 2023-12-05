module Y = Yojson.Basic.Util
module C = Continent
module R = Raylib

type owner = string

type t = {
  name : string;
  mutable troops : int;
  continent : string;
  location : string;
  neighbours : string list;
  mutable owner : owner;
}

let change_owner c s = c.owner <- s
let get_owner c = c.owner
let get_name c = c.name
let get_troops c = c.troops
let get_continent c = c.continent
let get_location c = c.location
let get_neighbours c = c.neighbours

let neighbours_to_string c =
  List.fold_left (fun acc t -> acc ^ t) "" c.neighbours

let init json : t =
  {
    name = json |> Y.member "name" |> Y.to_string;
    troops = 0;
    continent = json |> Y.member "continent" |> Y.to_string;
    location = json |> Y.member "tag" |> Y.to_string;
    neighbours =
      json |> Y.member "neighbors" |> Y.to_list |> List.map Y.to_string;
    owner = "";
  }

let add_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops + n

let subtract_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops - n
