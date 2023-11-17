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
(*--------------------------- Country Examples -------------------------------*)

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

let tests = "main test suite" >::: get_countries_list_tests
let _ = run_test_tt_main tests
