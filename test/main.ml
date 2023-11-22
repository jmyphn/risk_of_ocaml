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

let pp_country_list = pp_list Countries.get_name

let pp_country_option_array =
  pp_array (fun x ->
      match x with
      | Some y -> Countries.get_name y
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

(* North American Countries*)
let alaska = Countries.init "Alaska" north_america
let alberta = Countries.init "Alberta" north_america
let central_america = Countries.init "Central America" north_america
let eastern_us = Countries.init "Eastern US" north_america
let greenland = Countries.init "Greenland" north_america
let northwest_territory = Countries.init "Northwest Territory" north_america
let ontario = Countries.init "Ontario" north_america
let quebec = Countries.init "Quebec" north_america
let western_us = Countries.init "Western US" north_america

(* South American Countries *)
let argentina = Countries.init "Argentina" south_america
let brazil = Countries.init "Brazil" south_america
let venezuela = Countries.init "Venezuela" south_america
let peru = Countries.init "Peru" south_america

(* Australian Countries *)
let eastern_australia = Countries.init "Eastern Australia" australia
let indonesia = Countries.init "Indonesia" australia
let new_guinea = Countries.init "New Guinea" australia
let western_australia = Countries.init "Western Australia" australia

(* European Countries *)
let great_britain = Countries.init "Great Britain" europe
let iceland = Countries.init "Iceland" europe
let northern_europe = Countries.init "Northern Europe" europe
let scandinavia = Countries.init "Scandinavia" europe
let southern_europe = Countries.init "Southern Europe" europe
let ukraine = Countries.init "Ukraine" europe
let western_europe = Countries.init "Western Europe" europe

(* African Countries *)
let congo = Countries.init "Congo" africa
let east_africa = Countries.init "East Africa" africa
let egypt = Countries.init "Egypt" africa
let madagascar = Countries.init "Madagascar" africa
let north_africa = Countries.init "North Africa" africa
let south_africa = Countries.init "South Africa" africa

(* Asian Countries *)
let afghanistan = Countries.init "Afghanistan" asia
let china = Countries.init "China" asia
let india = Countries.init "India" asia
let irkutsk = Countries.init "Irkutsk" asia
let japan = Countries.init "Japan" asia
let kamchatka = Countries.init "Kamchatka" asia
let middle_east = Countries.init "Middle East" asia
let mongolia = Countries.init "Mongolia" asia
let siam = Countries.init "Siam" asia
let siberia = Countries.init "Siberia" asia
let ural = Countries.init "Ural" asia
let yakutsk = Countries.init "Yakutsk" asia

(* Set the neighboring countries for each of the above countries let _ =
   Countries.init_neighbors alaska [ northwest_territory; alberta; kamchatka ]

   let _ = Countries.init_neighbors alberta [ northwest_territory; alaska;
   ontario; western_us ] *)

(***************************** Countries tests ********************************)

(*----------------------------Countries.get_name -----------------------------*)
let countries_get_name_test out in1 _ =
  assert_equal out (Countries.get_name in1)

let countries_get_name_tests =
  [
    "get name of Alaska" >:: countries_get_name_test "Alaska" alaska;
    "get name of Alberta" >:: countries_get_name_test "Alberta" alberta;
    "get name of Central America"
    >:: countries_get_name_test "Central America" central_america;
    "get name of Eastern US" >:: countries_get_name_test "Eastern US" eastern_us;
    "get name of Greenland" >:: countries_get_name_test "Greenland" greenland;
    "get name of Argentina" >:: countries_get_name_test "Argentina" argentina;
    "get name of Brazil" >:: countries_get_name_test "Brazil" brazil;
    "get name of Venezuela" >:: countries_get_name_test "Venezuela" venezuela;
    "get name of Peru" >:: countries_get_name_test "Peru" peru;
    "get name of Eastern Australia"
    >:: countries_get_name_test "Eastern Australia" eastern_australia;
    "get name of Indonesia" >:: countries_get_name_test "Indonesia" indonesia;
    "get name of New Guinea" >:: countries_get_name_test "New Guinea" new_guinea;
    "get name of Western Australia"
    >:: countries_get_name_test "Western Australia" western_australia;
    "get name of Great Britain"
    >:: countries_get_name_test "Great Britain" great_britain;
    "get name of Iceland" >:: countries_get_name_test "Iceland" iceland;
    "get name of Northern Europe"
    >:: countries_get_name_test "Northern Europe" northern_europe;
    "get name of Scandinavia"
    >:: countries_get_name_test "Scandinavia" scandinavia;
    "get name of Southern Europe"
    >:: countries_get_name_test "Southern Europe" southern_europe;
    "get name of Congo" >:: countries_get_name_test "Congo" congo;
    "get name of East Africa"
    >:: countries_get_name_test "East Africa" east_africa;
    "get name of Egypt" >:: countries_get_name_test "Egypt" egypt;
    "get name of Afghanistan"
    >:: countries_get_name_test "Afghanistan" afghanistan;
    "get name of China" >:: countries_get_name_test "China" china;
    "get name of India" >:: countries_get_name_test "India" india;
    "get name of Irkutsk" >:: countries_get_name_test "Irkutsk" irkutsk;
    "get name of Japan" >:: countries_get_name_test "Japan" japan;
  ]

(*-----------------Countries.get_troops, add_value & subtract_value ----------*)
let _ = Countries.add_value 5 alaska
let _ = Countries.add_value 5 alberta
let _ = Countries.add_value 5 central_america
let _ = Countries.add_value 5 eastern_us
let _ = Countries.add_value 5 greenland
let _ = Countries.add_value 5 northwest_territory
let _ = Countries.add_value 5 ontario
let _ = Countries.add_value 5 quebec
let _ = Countries.add_value 5 western_us

let _ =
  Countries.add_value 5 argentina;
  Countries.subtract_value 3 argentina

let _ =
  Countries.add_value 5 brazil;
  Countries.subtract_value 3 brazil

let _ =
  Countries.add_value 5 venezuela;
  Countries.subtract_value 3 venezuela

let _ =
  Countries.add_value 5 peru;
  Countries.subtract_value 3 peru

let _ =
  Countries.add_value 7 eastern_australia;
  Countries.subtract_value 5 eastern_australia

let _ =
  Countries.add_value 7 indonesia;
  Countries.subtract_value 5 indonesia

let _ =
  Countries.add_value 7 new_guinea;
  Countries.subtract_value 5 new_guinea

let _ =
  Countries.add_value 7 western_australia;
  Countries.subtract_value 5 western_australia

let _ =
  Countries.add_value 10 great_britain;
  Countries.subtract_value 5 great_britain

let _ =
  Countries.add_value 10 iceland;
  Countries.subtract_value 5 iceland

let _ =
  Countries.add_value 10 northern_europe;
  Countries.subtract_value 5 northern_europe

let _ =
  Countries.add_value 10 scandinavia;
  Countries.subtract_value 5 scandinavia

let _ =
  Countries.add_value 10 southern_europe;
  Countries.subtract_value 5 southern_europe

let _ =
  Countries.add_value 10 ukraine;
  Countries.subtract_value 5 ukraine

let _ =
  Countries.add_value 10 western_europe;
  Countries.subtract_value 5 western_europe

let _ =
  Countries.add_value 1 congo;
  Countries.add_value 2 congo

let _ =
  Countries.add_value 1 east_africa;
  Countries.add_value 2 east_africa

let _ =
  Countries.add_value 1 egypt;
  Countries.add_value 2 egypt

let _ =
  Countries.add_value 1 madagascar;
  Countries.add_value 2 madagascar

let _ =
  Countries.add_value 1 north_africa;
  Countries.add_value 2 north_africa

let _ =
  Countries.add_value 1 south_africa;
  Countries.add_value 2 south_africa

let _ =
  Countries.add_value 4 afghanistan;
  Countries.add_value 3 afghanistan

let _ =
  Countries.add_value 4 china;
  Countries.add_value 3 china

let _ =
  Countries.add_value 4 india;
  Countries.add_value 3 india

let _ =
  Countries.add_value 4 irkutsk;
  Countries.add_value 3 irkutsk

let _ =
  Countries.add_value 4 japan;
  Countries.add_value 3 japan

let _ =
  Countries.add_value 4 kamchatka;
  Countries.add_value 3 kamchatka

let _ =
  Countries.add_value 4 middle_east;
  Countries.add_value 3 middle_east

let _ =
  Countries.add_value 4 mongolia;
  Countries.add_value 3 mongolia

let _ =
  Countries.add_value 4 siam;
  Countries.add_value 3 siam

let _ =
  Countries.add_value 4 siberia;
  Countries.add_value 3 siberia

let _ =
  Countries.add_value 4 ural;
  Countries.add_value 3 ural

let _ =
  Countries.add_value 4 yakutsk;
  Countries.add_value 3 yakutsk

let countries_get_troops_test out in1 _ =
  assert_equal out (Countries.get_troops in1)

let countries_troops_value_tests =
  [
    "get troops of Northwest Territory"
    >:: countries_get_troops_test 5 northwest_territory;
    "get troops of Alaska" >:: countries_get_troops_test 5 alaska;
    "get troops of Ontario" >:: countries_get_troops_test 5 ontario;
    "get troops of Quebec" >:: countries_get_troops_test 5 quebec;
    "get troops of Western US" >:: countries_get_troops_test 5 western_us;
    "get troops of Southern Europe"
    >:: countries_get_troops_test 5 southern_europe;
    "get troops of Ukraine" >:: countries_get_troops_test 5 ukraine;
    "get troops of Western Europe"
    >:: countries_get_troops_test 5 western_europe;
    "get troops of Madagascar" >:: countries_get_troops_test 3 madagascar;
    "get troops of North Africa" >:: countries_get_troops_test 3 north_africa;
    "get troops of South Africa" >:: countries_get_troops_test 3 south_africa;
    "get troops of Kamchatka" >:: countries_get_troops_test 7 kamchatka;
    "get troops of Middle East" >:: countries_get_troops_test 7 middle_east;
    "get troops of Mongolia" >:: countries_get_troops_test 7 mongolia;
    "get troops of Siam" >:: countries_get_troops_test 7 siam;
    "get troops of Siberia" >:: countries_get_troops_test 7 siberia;
    "get troops of Ural" >:: countries_get_troops_test 7 ural;
    "get troops of Yakutsk" >:: countries_get_troops_test 7 yakutsk;
    "get troops of Indonesia" >:: countries_get_troops_test 2 indonesia;
    "get troops of New Guinea" >:: countries_get_troops_test 2 new_guinea;
    "get troops of Western Australi"
    >:: countries_get_troops_test 2 western_australia;
    "get troops of Peru" >:: countries_get_troops_test 2 peru;
    "get troops of Brazil" >:: countries_get_troops_test 2 brazil;
    "get troops of Argentina" >:: countries_get_troops_test 2 argentina;
  ]

(*---------------------------Countries.get_continent -------------------------*)
let countries_get_continent_test out in1 _ =
  assert_equal out (Countries.get_continent in1)

let countries_get_continent_tests =
  [
    "get continent of Alberta"
    >:: countries_get_continent_test north_america alberta;
    "get continent of Greenland"
    >:: countries_get_continent_test north_america greenland;
    "get continent of Ontario"
    >:: countries_get_continent_test north_america ontario;
    "get continent of Quebec"
    >:: countries_get_continent_test north_america quebec;
    "get continent of Argentina"
    >:: countries_get_continent_test south_america argentina;
    "get continent of Venezuela"
    >:: countries_get_continent_test south_america venezuela;
    "get continent of Eastern Australia"
    >:: countries_get_continent_test australia eastern_australia;
    "get continent of Western Australia"
    >:: countries_get_continent_test australia western_australia;
    "get continent of Iceland" >:: countries_get_continent_test europe iceland;
    "get continent of Scandinavia"
    >:: countries_get_continent_test europe scandinavia;
    "get continent of Western Europe"
    >:: countries_get_continent_test europe western_europe;
    "get continent of Egypt" >:: countries_get_continent_test africa egypt;
    "get continent of Madagascar"
    >:: countries_get_continent_test africa madagascar;
    "get continent of Congo" >:: countries_get_continent_test africa congo;
    "get continent of Japan" >:: countries_get_continent_test asia japan;
    "get continent of Siam" >:: countries_get_continent_test asia siam;
    "get continent of Ural" >:: countries_get_continent_test asia ural;
    "get continent of Middle East"
    >:: countries_get_continent_test asia middle_east;
  ]

(*----------------------------Countries.get_location -------------------------*)
let countries_get_location_test out in1 _ =
  assert_equal out (Countries.get_location in1)

let countries_get_location_tests =
  [
    "get location of Alaska" >:: countries_get_location_test (0, 0) alaska;
    "get location of Central America"
    >:: countries_get_location_test (0, 0) central_america;
    "get location of Ontario" >:: countries_get_location_test (0, 0) ontario;
    "get location of Quebec" >:: countries_get_location_test (0, 0) quebec;
    "get location of Argentina" >:: countries_get_location_test (0, 0) argentina;
    "get location of Venezuela" >:: countries_get_location_test (0, 0) venezuela;
    "get location of Peru" >:: countries_get_location_test (0, 0) peru;
    "get location of Indonesia" >:: countries_get_location_test (0, 0) indonesia;
    "get location of New Guinea"
    >:: countries_get_location_test (0, 0) new_guinea;
    "get location of Eastern Australia"
    >:: countries_get_location_test (0, 0) eastern_australia;
    "get location of Great Britain"
    >:: countries_get_location_test (0, 0) great_britain;
    "get location of Northern Europe"
    >:: countries_get_location_test (0, 0) northern_europe;
    "get location of Ukraine" >:: countries_get_location_test (0, 0) ukraine;
    "get location of North Africa"
    >:: countries_get_location_test (0, 0) north_africa;
    "get location of South Africa"
    >:: countries_get_location_test (0, 0) south_africa;
    "get location of Egypt" >:: countries_get_location_test (0, 0) egypt;
    "get location of Irkutsk" >:: countries_get_location_test (0, 0) irkutsk;
    "get location of Yakutsk" >:: countries_get_location_test (0, 0) yakutsk;
    "get location of Afghanistan"
    >:: countries_get_location_test (0, 0) afghanistan;
  ]

(*----------------------------Countries.get_neighbors ------------------------*)
(* let countries_get_neighbors_test out in1 _ = assert_equal out
   (Countries.get_neighbours in1)

   let countries_get_neighbors_tests = [ "get neighbors of Alaska" >::
   countries_get_neighbors_test [ northwest_territory; alberta; kamchatka ]
   alaska; "get neighbors of Alberta" >:: countries_get_neighbors_test [
   northwest_territory; alaska; ontario; western_us ] alberta; ] *)

(*--------------------------- Player Examples --------------------------------*)
let p1 = Player.init Raylib.Color.gray

(***************************** Player tests **********************************)

(*-------------------------- Get_Countries_List ------------------------------*)
let get_countries_list_test out in1 _ =
  assert_equal ~printer:pp_country_list
    ~msg:
      ("function: get_countries_list\ninput: "
      ^ pp_country_option_array (Player.get_countries in1))
    out
    (Player.get_countries_lst in1)

let get_countries_list_tests =
  [ "get_countries_list empty" >:: get_countries_list_test [] p1 ]

(*------------------------------ add_country ---------------------------------*)
(* let add_country_test out in1 in2 _ = Player.add_country in1 in2; assert_equal
   ~printer:pp_country_option_array ~msg: ("function: add_country\ninput: " ^
   pp_country_option_array (Player.get_countries in1)) out (Player.get_countries
   in1)

   let add_country_test = [] *)

(******************************************************************************)

let tests =
  "main test suite"
  >::: List.flatten
         [
           get_countries_list_tests;
           continent_get_name_tests;
           continent_get_value_tests;
           countries_get_name_tests;
           countries_troops_value_tests;
           countries_get_continent_tests;
           countries_get_location_tests;
           (* countries_get_neighbors_tests; *)
         ]

let _ = run_test_tt_main tests
