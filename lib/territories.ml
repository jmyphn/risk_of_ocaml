type t = {
  name : string;
  mutable troops : int;
  continent : Continent.t;
  location : int * int;
  mutable neighbours : t list;
}

let get_name c = c.name
let get_troops c = c.troops
let get_continent c = c.continent
let get_location c = c.location
let get_neighbours c = c.neighbours

let init n c : t =
  { name = n; troops = 0; continent = c; location = (0, 0); neighbours = [] }

let init_neighbors c nbrs : unit = c.neighbours <- nbrs

let add_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops + n

let subtract_value (n : int) (territory : t) : unit =
  territory.troops <- territory.troops - n
