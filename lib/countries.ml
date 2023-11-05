type t = {
  name : string;
  mutable troops : int;
  continent : Continent.t;
  location : int * int;
  neighbours : t list;
}

let get_name c = c.name
let get_troops c = c.troops
let get_continent c = c.continent
let get_location c = c.location
let get_neighbours c = c.neighbours
let init () : t list = []

let add_value (n : int) (country : t) : unit =
  country.troops <- country.troops + n

let subtract_value (n : int) (country : t) : unit =
  country.troops <- country.troops - n
