type t = {
  name : string;
  color : Raylib.Color.t;
  territories : Territories.t option array;
}
(** AF: The record {name: _ ; color: _  ; Territories: _} represents a player. All [Some] 
   elements of the Territories array represents an territory owned by the player. If
   all of the elements of the Territories array are [None], they have lost the game.
   If they are all [Some], they have won the game.
   RI: The Territories array has a length of 42. All [Some] elements in the
   Territories array are to the left of all [None] elements. The Territories array
   has no duplicate territories. *)

(** Given a player [p], returns the name of the player. *)
let get_name (p : t) : string = p.name

(* Returns whether all elements in the array [a] are [None] *)
let are_none (a : Territories.t option array) : bool =
  Array.for_all
    (fun x ->
      match x with
      | None -> true
      | Some _ -> false)
    a

(* [are_some arr] returns true when all elements in [arr] are [Some x] and false
   when at least one element in the array is [None] *)
let are_some (arr : Territories.t option array) : bool =
  Array.for_all
    (fun x ->
      match x with
      | Some _ -> true
      | None -> false)
    arr

(* Given an array of options [arr], [find_index arr elt] returns the first index
   of the array that contains [elt]. Raises: ["Invalid_element"] if there is no
   [elt] in the array*)
let find_index (arr : Territories.t option array) (elt : Territories.t option) :
    int =
  let found = ref false in
  let acc = ref 0 in
  while !found = false && !acc <= Array.length arr - 1 do
    if arr.(!acc) = elt then found := true else acc := !acc + 1
  done;
  match !found with
  | true -> !acc
  | false -> failwith "Invalid_element"

(* Returns whether the array [a] satisfies: All [Some] elements in the
   Territories array are to the left of all [None] elements. *)
let check_inv (a : Territories.t option array) : bool =
  if are_some a then true
  else
    let idx = find_index a None in
    are_none (Array.sub a idx (Array.length a - idx))

(* [duplicate a] returns true when the array [a] has no duplicates and false
   when [a] contains duplicates *)
let duplicate (a : Territories.t option array) : bool =
  let lst = Array.to_list a in
  let u = List.sort_uniq Stdlib.compare lst in
  match List.compare_lengths lst u with
  | 0 -> true
  | _ -> false

(* Checks whether [p] satisfies the representation invariant. *)
let rep_ok (p : t) : unit =
  let ck1 = Array.length p.territories = 42 in
  let ck2 = check_inv p.territories in
  let ck3 = duplicate p.territories in
  if ck1 && ck2 && ck3 then () else ()

(** Given a player [p], returns the name of the player. *)
let get_color (p : t) : Raylib.Color.t = p.color

(** Given a player [p] and a string [s], returns the index of the territory with
    Territory.name = s. Raises: ["not owned"] if not found*)
let get_territory (p : t) (s : string) : Territories.t =
  let ter =
    Array.find_opt
      (fun t ->
        match t with
        | None -> false
        | Some t -> Territories.get_name t = s)
      p.territories
  in
  match ter with
  | None -> failwith "not owned"
  | Some None -> failwith "not owned"
  | Some (Some t) -> t

(** Given a player [p], returns an option array of Territories owned by the
    player. *)
let get_territories (p : t) : Territories.t option array = p.territories

(** Given a player [p], returns a list of Territories owned by the player. *)
let get_territories_lst (p : t) : Territories.t list =
  let lst = Array.to_list p.territories in
  List.filter_map (fun x -> x) lst

(** Given a player [p], adds a territory to the Territories array. Requires: [c]
    must not be owned by the player Raises: ["In_territory"] if [c] is already
    in the [p.territories] array *)
let add_territory (p : t) (c : Territories.t) : unit =
  if Array.exists (fun x -> x = Some c) p.territories then
    failwith "In_territory"
  else
    let idx = find_index p.territories None in
    p.territories.(idx) <- Some c;
    Territories.change_owner c p.name;
    rep_ok p

(** Given a player [p], removes a territory in the Territories array. Requires:
    [c] must be owned by the player and [p.territories] cannot be empty. Raises:
    ["Empty_Array"] if [p.territories] is empty*)
let remove_territory (p : t) (c : Territories.t) : unit =
  if Array.length p.territories = 0 then failwith "Empty_Array"
  else
    let idx1 = find_index p.territories (Some c) in
    let idx2 = find_index p.territories None in
    p.territories.(idx1) <- p.territories.(idx2 - 1);
    p.territories.(idx2 - 1) <- None;
    rep_ok p

(** Given player [p], returns the number of Territories in the Territories
    array.*)
let num_territories (p : t) : int =
  Array.fold_left
    (fun acc x -> if x = None then acc else acc + 1)
    0 p.territories

let territories_to_string (p : t) : string =
  Array.fold_left
    (fun acc x ->
      match x with
      | None -> acc ^ ""
      | Some x1 ->
          acc ^ Territories.get_name x1 ^ ": "
          ^ string_of_int (Territories.get_troops x1)
          ^ ", ")
    "" p.territories

(** Given a name [n] and color [c], initializes the player.*)
let init n c : t = { name = n; color = c; territories = Array.make 42 None }
