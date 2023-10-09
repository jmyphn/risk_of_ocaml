(*Game setup function*)
let setup () = print_endline ("start");;

(*Game loop function*)
let rec loop () = let s = read_line () in 
  if s = "end" then print_endline ("end") else 






  loop ()

(*initializes game*)
let () = setup () |> loop