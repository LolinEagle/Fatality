(* Prints all available keys/symbols from the automaton's alphabet
   Used to show the user what inputs are valid *)
let print_key_mappings alphabet =
  print_endline "Key mappings:";
  List.iter (fun key ->
    print_endline (Printf.sprintf "%s" key)
  ) alphabet;
  print_endline "____________________________________________________________"

(* Prints a message when a move sequence is successfully recognized
   Displays the entire recognized move as a space-separated string *)
let print_recognized_move move =
  print_endline (Printf.sprintf "%s recognized" (String.concat " " move))