module Y = Yojson.Basic.Util

type t = {
  name : string;
  value : int;
}

let get_name t = t.name
let get_value t = t.value

let create json : t =
  {
    name = json |> Y.member "name" |> Y.to_string;
    value = json |> Y.member "value" |> Y.to_int;
  }
(*let ownsV2 (lst : territory list) (p : player) : int = 0 *)
