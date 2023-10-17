
type t = {name : string; countries : Countries.t list}

let num_countries p = List.length p.countries

let get_name (c : t) : string = c.name 