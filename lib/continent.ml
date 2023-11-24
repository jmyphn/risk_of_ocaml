type t = {
  name : string;
  value : int;
}

let get_name t = t.name
let get_value t = t.value
let create (n : string) (num : int) : t = { name = n; value = num }

(*let ownsV2 (lst : territory list) (p : player) : int = 0 *)
