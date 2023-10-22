open Gamestate

(*Game setup function*)
let setup () = print_endline ""

(*Game loop function*)
let rec loop game_state () =
  let _ = Gamestate.message game_state in
  let _ = Gamestate.action game_state in
  let s = read_line () in
  if s = "end" then print_endline "end"
  else
    let new_state = change_state game_state s in
    loop new_state ()

(*initializes game*)
let () = setup () |> loop Gamestate.first_state
