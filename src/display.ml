let print_key_mappings alphabet =
  print_endline "Key mappings:";
  (* Iterating over each element in the alphabet list *)
  List.iter (fun key ->
    (* For each key, prints a formatted line showing the mapping *)
    print_endline (Printf.sprintf "%s" key)
  ) alphabet;
  print_endline "______________________________________________________________"

let print_recognized_move recognized automaton =
  if List.mem recognized automaton.Automaton.final_states then
    (* Try to find the combo description *)
    match List.assoc_opt recognized automaton.Automaton.combo_list with
    | Some description -> print_endline ("Combo executed: " ^ description)
    | None -> print_endline ("[Unknown combo - state " ^ string_of_int recognized ^ "]")
