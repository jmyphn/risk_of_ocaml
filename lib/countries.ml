type t = {
  name : string;
  troops : int;
  continent : Continent.t;
  location : int * int;
  neighbours : t list;
}

let get_name c = c.name
let get_troops c = c.troops
let get_continent c = c.continent
let get_location c = c.location
let get_neighbours c = c.neighbours

let init n c nbrs : t =
  { name = n; troops = 0; continent = c; location = (0, 0); neighbours = nbrs }

let add_value (n : int) (country : t) : t =
  { country with troops = country.troops + n }

let subtract_value (n : int) (country : t) : t =
  { country with troops = country.troops - n }
