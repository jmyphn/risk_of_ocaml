type t = {
  name : string;
  value : int;
}

let get_name t = t.name
let get_value t = t.value
let create (name : string) (n : int) : t = { name; value = n }

(*let ownsV2 (lst : country list) (p : player) : int = 0 *)
