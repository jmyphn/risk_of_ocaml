open OUnit2
open Lib

(***************************** Pretty Printers *******************************)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_list pp_elt arr] pretty-prints array [arr], using [pp_elt] to
    pretty-print each element of [arr]. *)
let pp_array pp_elt arr =
  let loop arr =
    let acc = ref "" in
    for i = 0 to Array.length arr - 1 do
      let str = pp_elt arr.(i) in
      if str = "" then acc := !acc else acc := !acc ^ str ^ "; "
    done;
    !acc
  in
  "{" ^ loop arr ^ "}"

let pp_country_list = pp_list Territories.get_name

let pp_country_option_array =
  pp_array (fun x ->
      match x with
      | Some y -> Territories.get_name y
      | None -> "")

(******************************** EXAMPLES ************************************)
(*--------------------------- Continent Examples -----------------------------*)

(* json file of the classic map *)
let path = "data/countries.json"
let json = Yojson.Basic.from_file path
let map = Map.create_map json

(* all territories *)
let territories = Map.get_territories map

(* all continents *)
(* let continents = Map.get_continents map *)

(* let get_continent_from_array name = match Array.find_opt (fun x ->
   Continent.get_name x = name) continents with | None -> failwith "bruh" | Some
   v -> v *)

let get_territory_from_array name =
  match Array.find_opt (fun x -> Territories.get_name x = name) territories with
  | None -> failwith "bruh"
  | Some v -> v

(* Create continents using Continent.of_string *)
(* let north_america = get_continent_from_array "North America" let
   south_america = get_continent_from_array "South America" let africa =
   get_continent_from_array "Africa" let europe = get_continent_from_array
   "Europe" let asia = get_continent_from_array "Asia" let australia =
   get_continent_from_array "Australia" *)

let north_america = Continent.of_string "North America"
let south_america = Continent.of_string "South America"
let africa = Continent.of_string "Africa"
let europe = Continent.of_string "Europe"
let asia = Continent.of_string "Asia"
let australia = Continent.of_string "Australia"

(***************************** Continent tests ********************************)

(*----------------------------Continent.get_name -----------------------------*)
let continent_get_name_test out in1 _ =
  assert_equal out (Continent.get_name in1)

let continent_get_name_tests =
  [
    "get name of North America"
    >:: continent_get_name_test North_America north_america;
    "get name of South America"
    >:: continent_get_name_test South_America south_america;
    "get name of Africa" >:: continent_get_name_test Africa africa;
    "get name of Europe" >:: continent_get_name_test Europe europe;
    "get name of Asia" >:: continent_get_name_test Asia asia;
    "get name of Australia" >:: continent_get_name_test Australia australia;
  ]

(*----------------------------Continent.get_value ----------------------------*)
let continent_get_value_test out in1 _ =
  assert_equal out (Continent.get_value in1)

let continent_get_value_tests =
  [
    "get value of North America" >:: continent_get_value_test 5 north_america;
    "get value of South America" >:: continent_get_value_test 2 south_america;
    "get value of Africa" >:: continent_get_value_test 3 africa;
    "get value of Europe" >:: continent_get_value_test 5 europe;
    "get value of Asia" >:: continent_get_value_test 7 asia;
    "get value of Australia" >:: continent_get_value_test 2 australia;
  ]

(*----------------------------Continent.to_string ----------------------------*)
let continent_to_string_test out in1 _ =
  assert_equal out (Continent.to_string (Continent.get_name in1))

let continent_to_string_tests =
  [
    "to_string North America"
    >:: continent_to_string_test "North America" north_america;
    "to_string South America"
    >:: continent_to_string_test "South America" south_america;
    "to_string Africa" >:: continent_to_string_test "Africa" africa;
    "to_string Europe" >:: continent_to_string_test "Europe" europe;
    "to_string Asia" >:: continent_to_string_test "Asia" asia;
    "to_string Australia" >:: continent_to_string_test "Australia" australia;
  ]

(*--------------------------- Country Examples -------------------------------*)

(* North American territories*)
let alaska = get_territory_from_array "Alaska"
let alberta = get_territory_from_array "Alberta"
let central_america = get_territory_from_array "Central America"
let eastern_us = get_territory_from_array "Eastern US"
let greenland = get_territory_from_array "Greenland"
let northwest_territory = get_territory_from_array "Northwest Territory"
let ontario = get_territory_from_array "Ontario"
let quebec = get_territory_from_array "Quebec"
let western_us = get_territory_from_array "Western US"

(* South American territories *)
let argentina = get_territory_from_array "Argentina"
let brazil = get_territory_from_array "Brazil"
let venezuela = get_territory_from_array "Venezuela"
let peru = get_territory_from_array "Peru"

(* Australian territories *)
let eastern_australia = get_territory_from_array "Eastern Australia"
let indonesia = get_territory_from_array "Indonesia"
let new_guinea = get_territory_from_array "New Guinea"
let western_australia = get_territory_from_array "Western Australia"

(* European territories *)
let great_britain = get_territory_from_array "Great Britain"
let iceland = get_territory_from_array "Iceland"
let northern_europe = get_territory_from_array "Northern Europe"
let scandinavia = get_territory_from_array "Scandinavia"
let southern_europe = get_territory_from_array "Southern Europe"
let ukraine = get_territory_from_array "Ukraine"
let western_europe = get_territory_from_array "Western Europe"

(* African territories *)
let congo = get_territory_from_array "Congo"
let east_africa = get_territory_from_array "East Africa"
let egypt = get_territory_from_array "Egypt"
let madagascar = get_territory_from_array "Madagascar"
let north_africa = get_territory_from_array "North Africa"
let south_africa = get_territory_from_array "South Africa"

(* Asian territories *)
let afghanistan = get_territory_from_array "Afghanistan"
let china = get_territory_from_array "China"
let india = get_territory_from_array "India"
let irkutsk = get_territory_from_array "Irkutsk"
let japan = get_territory_from_array "Japan"
let kamchatka = get_territory_from_array "Kamchatka"
let middle_east = get_territory_from_array "Middle East"
let mongolia = get_territory_from_array "Mongolia"
let siam = get_territory_from_array "Siam"
let siberia = get_territory_from_array "Siberia"
let ural = get_territory_from_array "Ural"
let yakutsk = get_territory_from_array "Yakutsk"

(* Set the neighboring territories for each of the above territories let _ =
   Territories.init_neighbors alaska [ northwest_territory; alberta; kamchatka ]

   let _ = Territories.init_neighbors alberta [ northwest_territory; alaska;
   ontario; western_us ] *)

(***************************** Territories tests ****************************)

(*---------------------Territories.change_owner & get_owner ----------------*)

let _ = Territories.change_owner ontario "1"
let _ = Territories.change_owner siam "2"
let _ = Territories.change_owner alaska "3"
let _ = Territories.change_owner alberta "4"
let _ = Territories.change_owner congo "5"
let _ = Territories.change_owner indonesia "6"
let _ = Territories.change_owner peru "1"
let _ = Territories.change_owner kamchatka "2"
let _ = Territories.change_owner new_guinea "3"
let _ = Territories.change_owner great_britain "4"
let _ = Territories.change_owner scandinavia "5"
let _ = Territories.change_owner egypt "6"

let territories_owner_test out in1 _ =
  assert_equal out (Territories.get_owner in1)

let territories_owner_tests =
  [
    "get owner of ontario" >:: territories_owner_test "1" ontario;
    "get owner of siam" >:: territories_owner_test "2" siam;
    "get owner of alaska" >:: territories_owner_test "3" alaska;
    "get owner of alberta" >:: territories_owner_test "4" alberta;
    "get owner of congo" >:: territories_owner_test "5" congo;
    "get owner of indonesia" >:: territories_owner_test "6" indonesia;
    "get owner of peru" >:: territories_owner_test "1" peru;
    "get owner of kamchatka" >:: territories_owner_test "2" kamchatka;
    "get owner of new guinea" >:: territories_owner_test "3" new_guinea;
    "get owner of great britain" >:: territories_owner_test "4" great_britain;
    "get owner of scandinavia" >:: territories_owner_test "5" scandinavia;
    "get owner of egypt" >:: territories_owner_test "6" egypt;
  ]

(*----------------------------Territories.get_name -------------------------*)
let territories_get_name_test out in1 _ =
  assert_equal out (Territories.get_name in1)

let territories_get_name_tests =
  [
    "get name of Alaska" >:: territories_get_name_test "Alaska" alaska;
    "get name of Alberta" >:: territories_get_name_test "Alberta" alberta;
    "get name of Central America"
    >:: territories_get_name_test "Central America" central_america;
    "get name of Eastern US"
    >:: territories_get_name_test "Eastern US" eastern_us;
    "get name of Greenland" >:: territories_get_name_test "Greenland" greenland;
    "get name of Argentina" >:: territories_get_name_test "Argentina" argentina;
    "get name of Brazil" >:: territories_get_name_test "Brazil" brazil;
    "get name of Venezuela" >:: territories_get_name_test "Venezuela" venezuela;
    "get name of Peru" >:: territories_get_name_test "Peru" peru;
    "get name of Eastern Australia"
    >:: territories_get_name_test "Eastern Australia" eastern_australia;
    "get name of Indonesia" >:: territories_get_name_test "Indonesia" indonesia;
    "get name of New Guinea"
    >:: territories_get_name_test "New Guinea" new_guinea;
    "get name of Western Australia"
    >:: territories_get_name_test "Western Australia" western_australia;
    "get name of Great Britain"
    >:: territories_get_name_test "Great Britain" great_britain;
    "get name of Iceland" >:: territories_get_name_test "Iceland" iceland;
    "get name of Northern Europe"
    >:: territories_get_name_test "Northern Europe" northern_europe;
    "get name of Scandinavia"
    >:: territories_get_name_test "Scandinavia" scandinavia;
    "get name of Southern Europe"
    >:: territories_get_name_test "Southern Europe" southern_europe;
    "get name of Congo" >:: territories_get_name_test "Congo" congo;
    "get name of East Africa"
    >:: territories_get_name_test "East Africa" east_africa;
    "get name of Egypt" >:: territories_get_name_test "Egypt" egypt;
    "get name of Afghanistan"
    >:: territories_get_name_test "Afghanistan" afghanistan;
    "get name of China" >:: territories_get_name_test "China" china;
    "get name of India" >:: territories_get_name_test "India" india;
    "get name of Irkutsk" >:: territories_get_name_test "Irkutsk" irkutsk;
    "get name of Japan" >:: territories_get_name_test "Japan" japan;
  ]

(*------------Territories.get_troops, add_value & subtract_value ----------*)
let _ = Territories.add_value 5 alaska
let _ = Territories.add_value 5 alberta
let _ = Territories.add_value 5 central_america
let _ = Territories.add_value 5 eastern_us
let _ = Territories.add_value 5 greenland
let _ = Territories.add_value 5 northwest_territory
let _ = Territories.add_value 5 ontario
let _ = Territories.add_value 5 quebec
let _ = Territories.add_value 5 western_us

let _ =
  Territories.add_value 5 argentina;
  Territories.subtract_value 3 argentina

let _ =
  Territories.add_value 5 brazil;
  Territories.subtract_value 3 brazil

let _ =
  Territories.add_value 5 venezuela;
  Territories.subtract_value 3 venezuela

let _ =
  Territories.add_value 5 peru;
  Territories.subtract_value 3 peru

let _ =
  Territories.add_value 7 eastern_australia;
  Territories.subtract_value 5 eastern_australia

let _ =
  Territories.add_value 7 indonesia;
  Territories.subtract_value 5 indonesia

let _ =
  Territories.add_value 7 new_guinea;
  Territories.subtract_value 5 new_guinea

let _ =
  Territories.add_value 7 western_australia;
  Territories.subtract_value 5 western_australia

let _ =
  Territories.add_value 10 great_britain;
  Territories.subtract_value 5 great_britain

let _ =
  Territories.add_value 10 iceland;
  Territories.subtract_value 5 iceland

let _ =
  Territories.add_value 10 northern_europe;
  Territories.subtract_value 5 northern_europe

let _ =
  Territories.add_value 10 scandinavia;
  Territories.subtract_value 5 scandinavia

let _ =
  Territories.add_value 10 southern_europe;
  Territories.subtract_value 5 southern_europe

let _ =
  Territories.add_value 10 ukraine;
  Territories.subtract_value 5 ukraine

let _ =
  Territories.add_value 10 western_europe;
  Territories.subtract_value 5 western_europe

let _ =
  Territories.add_value 1 congo;
  Territories.add_value 2 congo

let _ =
  Territories.add_value 1 east_africa;
  Territories.add_value 2 east_africa

let _ =
  Territories.add_value 1 egypt;
  Territories.add_value 2 egypt

let _ =
  Territories.add_value 1 madagascar;
  Territories.add_value 2 madagascar

let _ =
  Territories.add_value 1 north_africa;
  Territories.add_value 2 north_africa

let _ =
  Territories.add_value 1 south_africa;
  Territories.add_value 2 south_africa

let _ =
  Territories.add_value 4 afghanistan;
  Territories.add_value 3 afghanistan

let _ =
  Territories.add_value 4 china;
  Territories.add_value 3 china

let _ =
  Territories.add_value 4 india;
  Territories.add_value 3 india

let _ =
  Territories.add_value 4 irkutsk;
  Territories.add_value 3 irkutsk

let _ =
  Territories.add_value 4 japan;
  Territories.add_value 3 japan

let _ =
  Territories.add_value 4 kamchatka;
  Territories.add_value 3 kamchatka

let _ =
  Territories.add_value 4 middle_east;
  Territories.add_value 3 middle_east

let _ =
  Territories.add_value 4 mongolia;
  Territories.add_value 3 mongolia

let _ =
  Territories.add_value 4 siam;
  Territories.add_value 3 siam

let _ =
  Territories.add_value 4 siberia;
  Territories.add_value 3 siberia

let _ =
  Territories.add_value 4 ural;
  Territories.add_value 3 ural

let _ =
  Territories.add_value 4 yakutsk;
  Territories.add_value 3 yakutsk

let territories_get_troops_test out in1 _ =
  assert_equal out (Territories.get_troops in1)

let territories_troops_value_tests =
  [
    "get troops of Northwest Territory"
    >:: territories_get_troops_test 5 northwest_territory;
    "get troops of Alaska" >:: territories_get_troops_test 5 alaska;
    "get troops of Ontario" >:: territories_get_troops_test 5 ontario;
    "get troops of Quebec" >:: territories_get_troops_test 5 quebec;
    "get troops of Western US" >:: territories_get_troops_test 5 western_us;
    "get troops of Southern Europe"
    >:: territories_get_troops_test 5 southern_europe;
    "get troops of Ukraine" >:: territories_get_troops_test 5 ukraine;
    "get troops of Western Europe"
    >:: territories_get_troops_test 5 western_europe;
    "get troops of Madagascar" >:: territories_get_troops_test 3 madagascar;
    "get troops of North Africa" >:: territories_get_troops_test 3 north_africa;
    "get troops of South Africa" >:: territories_get_troops_test 3 south_africa;
    "get troops of Kamchatka" >:: territories_get_troops_test 7 kamchatka;
    "get troops of Middle East" >:: territories_get_troops_test 7 middle_east;
    "get troops of Mongolia" >:: territories_get_troops_test 7 mongolia;
    "get troops of Siam" >:: territories_get_troops_test 7 siam;
    "get troops of Siberia" >:: territories_get_troops_test 7 siberia;
    "get troops of Ural" >:: territories_get_troops_test 7 ural;
    "get troops of Yakutsk" >:: territories_get_troops_test 7 yakutsk;
    "get troops of Indonesia" >:: territories_get_troops_test 2 indonesia;
    "get troops of New Guinea" >:: territories_get_troops_test 2 new_guinea;
    "get troops of Western Australi"
    >:: territories_get_troops_test 2 western_australia;
    "get troops of Peru" >:: territories_get_troops_test 2 peru;
    "get troops of Brazil" >:: territories_get_troops_test 2 brazil;
    "get troops of Argentina" >:: territories_get_troops_test 2 argentina;
  ]

(*------------------------Territories.get_continent -------------------------*)
let territories_get_continent_test out in1 _ =
  assert_equal out (Territories.get_continent in1)

let territories_get_continent_tests =
  [
    "get continent of Alberta"
    >:: territories_get_continent_test north_america alberta;
    "get continent of Greenland"
    >:: territories_get_continent_test north_america greenland;
    "get continent of Ontario"
    >:: territories_get_continent_test north_america ontario;
    "get continent of Quebec"
    >:: territories_get_continent_test north_america quebec;
    "get continent of Argentina"
    >:: territories_get_continent_test south_america argentina;
    "get continent of Venezuela"
    >:: territories_get_continent_test south_america venezuela;
    "get continent of Eastern Australia"
    >:: territories_get_continent_test australia eastern_australia;
    "get continent of Western Australia"
    >:: territories_get_continent_test australia western_australia;
    "get continent of Iceland" >:: territories_get_continent_test europe iceland;
    "get continent of Scandinavia"
    >:: territories_get_continent_test europe scandinavia;
    "get continent of Western Europe"
    >:: territories_get_continent_test europe western_europe;
    "get continent of Egypt" >:: territories_get_continent_test africa egypt;
    "get continent of Madagascar"
    >:: territories_get_continent_test africa madagascar;
    "get continent of Congo" >:: territories_get_continent_test africa congo;
    "get continent of Japan" >:: territories_get_continent_test asia japan;
    "get continent of Siam" >:: territories_get_continent_test asia siam;
    "get continent of Ural" >:: territories_get_continent_test asia ural;
    "get continent of Middle East"
    >:: territories_get_continent_test asia middle_east;
  ]

(*-------------------------Territories.get_location -------------------------*)
let territories_get_location_test out in1 _ =
  assert_equal out (Territories.get_location in1)

let territories_get_location_tests =
  [
    "get location of Alaska"
    >:: territories_get_location_test "_alaska.png" alaska;
    "get location of Central America"
    >:: territories_get_location_test "_ca.png" central_america;
    "get location of Ontario"
    >:: territories_get_location_test "_ontario.png" ontario;
    "get location of Quebec"
    >:: territories_get_location_test "_quebec.png" quebec;
    "get location of Argentina"
    >:: territories_get_location_test "_arg.png" argentina;
    "get location of Venezuela"
    >:: territories_get_location_test "_venezuela.png" venezuela;
    "get location of Peru" >:: territories_get_location_test "_peru.png" peru;
    "get location of Indonesia"
    >:: territories_get_location_test "_indonesia.png" indonesia;
    "get location of New Guinea"
    >:: territories_get_location_test "_new_guinea.png" new_guinea;
    "get location of Eastern Australia"
    >:: territories_get_location_test "_east_australia.png" eastern_australia;
    "get location of Great Britain"
    >:: territories_get_location_test "_great_britain.png" great_britain;
    "get location of Northern Europe"
    >:: territories_get_location_test "_n_europe.png" northern_europe;
    "get location of Ukraine"
    >:: territories_get_location_test "_ukraine.png" ukraine;
    "get location of North Africa"
    >:: territories_get_location_test "_n_africa.png" north_africa;
    "get location of South Africa"
    >:: territories_get_location_test "_s_africa.png" south_africa;
    "get location of Egypt" >:: territories_get_location_test "_egypt.png" egypt;
    "get location of Irkutsk"
    >:: territories_get_location_test "_irkutsk.png" irkutsk;
    "get location of Yakutsk"
    >:: territories_get_location_test "_yakutsk.png" yakutsk;
    "get location of Afghanistan"
    >:: territories_get_location_test "_afghanistan.png" afghanistan;
  ]

(*--------------------------Territories.get_neighbors ------------------------*)
(* let territories_get_neighbors_test out in1 _ = assert_equal out
   (Territories.get_neighbours in1)

   let territories_get_neighbors_tests = [ "get neighbors of Alaska" >::
   territories_get_neighbors_test [ northwest_territory; alberta; kamchatka ]
   alaska; "get neighbors of Alberta" >:: territories_get_neighbors_test [
   northwest_territory; alaska; ontario; western_us ] alberta; ] *)

(*--------------------------- Player Examples --------------------------------*)
let p1 = Player.init "temp" Raylib.Color.gray

(***************************** Player tests **********************************)

(*-------------------------- Get_territories_List
  ------------------------------*)
let get_territories_list_test out in1 _ =
  assert_equal ~printer:pp_country_list
    ~msg:
      ("function: get_territories_list\ninput: "
      ^ pp_country_option_array (Player.get_territories in1))
    out
    (Player.get_territories_lst in1)

let get_territories_list_tests =
  [ "get_territories_list empty" >:: get_territories_list_test [] p1 ]

(*------------------------------ add_country ---------------------------------*)
(* let add_country_test out in1 in2 _ = Player.add_country in1 in2; assert_equal
   ~printer:pp_country_option_array ~msg: ("function: add_country\ninput: " ^
   pp_country_option_array (Player.get_territories in1)) out
   (Player.get_territories in1)

   let add_country_test = [] *)

(******************************************************************************)

let tests =
  "main test suite"
  >::: List.flatten
         [
           get_territories_list_tests;
           continent_get_name_tests;
           continent_get_value_tests;
           continent_to_string_tests;
           territories_owner_tests;
           territories_get_name_tests;
           territories_troops_value_tests;
           territories_get_continent_tests;
           territories_get_location_tests;
           (* territories_get_neighbors_tests; *)
         ]

let _ = run_test_tt_main tests
