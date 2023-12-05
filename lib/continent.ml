module Y = Yojson.Basic.Util

type continents =
  | Europe
  | Asia
  | Africa
  | Australia
  | North_America
  | South_America

type t = {
  name : continents;
  value : int;
}

let get_name t = t.name
let get_value t = t.value

let to_string = function
  | Europe -> "Europe"
  | Asia -> "Asia"
  | Africa -> "Africa"
  | Australia -> "Australia"
  | North_America -> "North America"
  | South_America -> "South America"

let of_string = function
  | "Europe" -> { name = Europe; value = 3 }
  | "Asia" -> { name = Asia; value = 3 }
  | "Africa" -> { name = Africa; value = 3 }
  | "Australia" -> { name = Australia; value = 3 }
  | "North America" -> { name = North_America; value = 3 }
  | "South America" -> { name = South_America; value = 3 }
  | _ -> failwith "Invalid continent string"

(* let create json : t = let name_str = json |> Y.member "name" |> Y.to_string
   in let name = try of_string name_str with Failure _ -> failwith "Invalid\n
   continent in JSON" in { name; value = json |> Y.member "value" |> Y.to_int
   } *)
(*let ownsV2 (lst : territory list) (p : player) : int = 0 *)
