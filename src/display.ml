let print_key_mappings alphabet =
  print_endline "Key mappings:";
  (* Iterating over each element in the alphabet list *)
  List.iter (fun key ->
    (* For each key, prints a formatted line showing the mapping *)
    print_endline (Printf.sprintf "%s" key)
  ) alphabet;
  print_endline "____________________________________________________________"

let print_recognized_move move =
  print_endline (Printf.sprintf "%s recognized" (String.concat " " move))
