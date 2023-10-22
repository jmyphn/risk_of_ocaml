type t = { name : string; value : int }

let get_name c = c.name
let get_value c = c.value
let init (n : string) (troops : int) : t = { name = n; value = troops }

let add_value (n : int) (country : t) : t =
  { country with value = country.value + n }
