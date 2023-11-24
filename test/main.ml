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
let north_america = Continent.create "North America" 5
let south_america = Continent.create "South America" 2
let africa = Continent.create "Africa" 3
let europe = Continent.create "Europe" 5
let asia = Continent.create "Asia" 7
let australia = Continent.create "Australia" 2

(***************************** Continent tests ********************************)

(*----------------------------Continent.get_name -----------------------------*)
let continent_get_name_test out in1 _ =
  assert_equal out (Continent.get_name in1)

let continent_get_name_tests =
  [
    "get name of North America"
    >:: continent_get_name_test "North America" north_america;
    "get name of South America"
    >:: continent_get_name_test "South America" south_america;
    "get name of Africa" >:: continent_get_name_test "Africa" africa;
    "get name of Europe" >:: continent_get_name_test "Europe" europe;
    "get name of Asia" >:: continent_get_name_test "Asia" asia;
    "get name of Australia" >:: continent_get_name_test "Australia" australia;
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

(*--------------------------- Country Examples -------------------------------*)

(* North American territories*)
let alaska = Territories.init "Alaska" north_america
let alberta = Territories.init "Alberta" north_america
let central_america = Territories.init "Central America" north_america
let eastern_us = Territories.init "Eastern US" north_america
let greenland = Territories.init "Greenland" north_america
let northwest_territory = Territories.init "Northwest Territory" north_america
let ontario = Territories.init "Ontario" north_america
let quebec = Territories.init "Quebec" north_america
let western_us = Territories.init "Western US" north_america

(* South American territories *)
let argentina = Territories.init "Argentina" south_america
let brazil = Territories.init "Brazil" south_america
let venezuela = Territories.init "Venezuela" south_america
let peru = Territories.init "Peru" south_america

(* Australian territories *)
let eastern_australia = Territories.init "Eastern Australia" australia
let indonesia = Territories.init "Indonesia" australia
let new_guinea = Territories.init "New Guinea" australia
let western_australia = Territories.init "Western Australia" australia

(* European territories *)
let great_britain = Territories.init "Great Britain" europe
let iceland = Territories.init "Iceland" europe
let northern_europe = Territories.init "Northern Europe" europe
let scandinavia = Territories.init "Scandinavia" europe
let southern_europe = Territories.init "Southern Europe" europe
let ukraine = Territories.init "Ukraine" europe
let western_europe = Territories.init "Western Europe" europe

(* African territories *)
let congo = Territories.init "Congo" africa
let east_africa = Territories.init "East Africa" africa
let egypt = Territories.init "Egypt" africa
let madagascar = Territories.init "Madagascar" africa
let north_africa = Territories.init "North Africa" africa
let south_africa = Territories.init "South Africa" africa

(* Asian territories *)
let afghanistan = Territories.init "Afghanistan" asia
let china = Territories.init "China" asia
let india = Territories.init "India" asia
let irkutsk = Territories.init "Irkutsk" asia
let japan = Territories.init "Japan" asia
let kamchatka = Territories.init "Kamchatka" asia
let middle_east = Territories.init "Middle East" asia
let mongolia = Territories.init "Mongolia" asia
let siam = Territories.init "Siam" asia
let siberia = Territories.init "Siberia" asia
let ural = Territories.init "Ural" asia
let yakutsk = Territories.init "Yakutsk" asia

(* Set the neighboring territories for each of the above territories let _ =
   Territories.init_neighbors alaska [ northwest_territory; alberta; kamchatka ]

   let _ = Territories.init_neighbors alberta [ northwest_territory; alaska;
   ontario; western_us ] *)

(***************************** territories tests
  ********************************)

(*----------------------------Territories.get_name
  -----------------------------*)
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

(*-----------------Territories.get_troops, add_value & subtract_value
  ----------*)
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

(*---------------------------Territories.get_continent
  -------------------------*)
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

(*----------------------------Territories.get_location
  -------------------------*)
let territories_get_location_test out in1 _ =
  assert_equal out (Territories.get_location in1)

let territories_get_location_tests =
  [
    "get location of Alaska" >:: territories_get_location_test (0, 0) alaska;
    "get location of Central America"
    >:: territories_get_location_test (0, 0) central_america;
    "get location of Ontario" >:: territories_get_location_test (0, 0) ontario;
    "get location of Quebec" >:: territories_get_location_test (0, 0) quebec;
    "get location of Argentina"
    >:: territories_get_location_test (0, 0) argentina;
    "get location of Venezuela"
    >:: territories_get_location_test (0, 0) venezuela;
    "get location of Peru" >:: territories_get_location_test (0, 0) peru;
    "get location of Indonesia"
    >:: territories_get_location_test (0, 0) indonesia;
    "get location of New Guinea"
    >:: territories_get_location_test (0, 0) new_guinea;
    "get location of Eastern Australia"
    >:: territories_get_location_test (0, 0) eastern_australia;
    "get location of Great Britain"
    >:: territories_get_location_test (0, 0) great_britain;
    "get location of Northern Europe"
    >:: territories_get_location_test (0, 0) northern_europe;
    "get location of Ukraine" >:: territories_get_location_test (0, 0) ukraine;
    "get location of North Africa"
    >:: territories_get_location_test (0, 0) north_africa;
    "get location of South Africa"
    >:: territories_get_location_test (0, 0) south_africa;
    "get location of Egypt" >:: territories_get_location_test (0, 0) egypt;
    "get location of Irkutsk" >:: territories_get_location_test (0, 0) irkutsk;
    "get location of Yakutsk" >:: territories_get_location_test (0, 0) yakutsk;
    "get location of Afghanistan"
    >:: territories_get_location_test (0, 0) afghanistan;
  ]

(*----------------------------Territories.get_neighbors
  ------------------------*)
(* let territories_get_neighbors_test out in1 _ = assert_equal out
   (Territories.get_neighbours in1)

   let territories_get_neighbors_tests = [ "get neighbors of Alaska" >::
   territories_get_neighbors_test [ northwest_territory; alberta; kamchatka ]
   alaska; "get neighbors of Alberta" >:: territories_get_neighbors_test [
   northwest_territory; alaska; ontario; western_us ] alberta; ] *)

(*--------------------------- Player Examples --------------------------------*)
let p1 = Player.init Raylib.Color.gray

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
           territories_get_name_tests;
           territories_troops_value_tests;
           territories_get_continent_tests;
           territories_get_location_tests;
           (* territories_get_neighbors_tests; *)
         ]

let _ = run_test_tt_main tests
