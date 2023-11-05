type t = {
  name : string;
  color : Raylib.Color.t;
  mutable countries : Countries.t option array;
}
(** AF: The record {name: _ ; color: _  ; countries: _} represents a player. All [Some] 
   elements of the Countries array represents an country owned by the player. If
   all of the elements of the countries array are [None], they have lost the game.
   If they are all [Some], they have won the game.
   RI: The countries array has a length of 42. All [Some] elements in the
   countries array are to the left of all [None] elements. The countries array
   has no duplicate territories. *)

(** Given a player [p], returns the name of the player. *)
let get_name (p : t) : string = p.name

(* Returns whether all elements in the array [a] are [None] *)
let are_none (a : Countries.t option array) : bool =
  Array.for_all
    (fun x ->
      match x with
      | None -> true
      | Some _ -> false)
    a

(* [are_some arr] returns true when all elements in [arr] are [Some x] and false
   when at least one element in the array is [None] *)
let are_some (arr : Countries.t option array) : bool =
  Array.for_all
    (fun x ->
      match x with
      | Some _ -> true
      | None -> false)
    arr

(* Given an array of options [arr], [find_index arr elt] returns the first index
   of the array that contains [elt]. Raises: ["Invalid_element"] if there is no
   [elt] in the array*)
let find_index (arr : Countries.t option array) (elt : Countries.t option) : int
    =
  let found = ref false in
  let acc = ref 0 in
  while !found = false && !acc <= Array.length arr - 1 do
    if arr.(!acc) = elt then found := true else acc := !acc + 1
  done;
  match !found with
  | true -> !acc
  | false -> failwith "Invalid_element"

(* Returns whether the array [a] satisfies: All [Some] elements in the countries
   array are to the left of all [None] elements. *)
let check_inv (a : Countries.t option array) : bool =
  if are_some a then true
  else
    let idx = find_index a None in
    are_none (Array.sub a idx (Array.length a - idx))

(* [duplicate a] returns true when the array [a] has no duplicates and false
   when [a] contains duplicates *)
let duplicate (a : Countries.t option array) : bool =
  let lst = Array.to_list a in
  let u = List.sort_uniq Stdlib.compare lst in
  match List.compare_lengths lst u with
  | 0 -> true
  | _ -> false

(* Checks whether [p] satisfies the representation invariant. *)
let rep_ok (p : t) : unit =
  let ck1 = Array.length p.countries = 42 in
  let ck2 = check_inv p.countries in
  let ck3 = duplicate p.countries in
  if ck1 && ck2 && ck3 then () else failwith "RI Violated"

(** Given a player [p], returns the name of the player. *)
let get_color (p : t) : Raylib.Color.t = p.color

(** Given a player [p], returns an option array of countries owned by the
    player. *)
let get_countries (p : t) : Countries.t option array = p.countries

(** Given a player [p], returns a list of countries owned by the player. *)
let get_countries_lst (p : t) : Countries.t list =
  let lst = Array.to_list p.countries in
  List.filter_map (fun x -> x) lst

(** Given a player [p], adds a country to the countries array. Requires: [c]
    must not be owned by the player Raises: ["In_country"] if [c] is already in
    the [p.countries] array *)
let add_country (p : t) (c : Countries.t) : unit =
  if Array.exists (fun x -> x = Some c) p.countries then failwith "In_country"
  else
    let idx = find_index p.countries None in
    p.countries.(idx) <- Some c;
    rep_ok p

(** Given a player [p], removes a country in the countries array. Requires: [c]
    must be owned by the player and [p.countries] cannot be empty. Raises:
    ["Empty_Array"] if [p.countries] is empty*)
let remove_country (p : t) (c : Countries.t) : unit =
  if Array.length p.countries = 0 then failwith "Empty_Array"
  else
    let idx1 = find_index p.countries (Some c) in
    let idx2 = find_index p.countries None in
    p.countries.(idx1) <- p.countries.(idx2 - 1);
    p.countries.(idx2 - 1) <- None;
    rep_ok p

(** Given player [p], returns the number of countries in the countries array.*)
let num_countries (p : t) : int =
  Array.fold_left (fun acc x -> if x = None then acc else acc + 1) 0 p.countries

(** Given a name [n], initializes the player.*)
let init c : t =
  { name = "temporary"; color = c; countries = Array.make 42 None }
