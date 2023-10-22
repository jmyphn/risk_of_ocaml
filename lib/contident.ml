type t = {
  name : string;
  value : int;
}

let get_name t = t.name
let get_value t = t.value

let create (name : string) : t =
  {
    name;
    value =
      (match name with
      | "North America" -> 5
      | "South America" -> 4
      | "Africa" -> 4
      | "Europe" -> 5
      | "Asia" -> 8
      | "Australia" -> 2
      | _ -> 0);
  }

(*let ownsV2 (lst : country list) (p : player) : int = 0 *)
