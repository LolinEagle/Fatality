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

let generate_mermaid_diagram automaton filename =
  (* Ensure automaton directory exists *)
  let automaton_dir = "automaton" in
  if not (Sys.file_exists automaton_dir) then
    Sys.mkdir automaton_dir 0o755;
  
  (* Create or open file for writing in the automaton directory *)
  let filepath = Filename.concat automaton_dir filename in
  let oc = open_out filepath in
  
  (* Write markdown header *)
  Printf.fprintf oc "# Automaton Visualization\n\n";
  Printf.fprintf oc "```mermaid\nflowchart LR\n";
  
  (* Style for states *)
  List.iter (fun state ->
    (* Regular state *)
    Printf.fprintf oc "    %d([%d])\n" state state;
    
    (* Mark initial state with different style *)
    if state = automaton.Automaton.initial_state then
      Printf.fprintf oc "    style %d fill:#8af,stroke:#333,stroke-width:2px\n" state;
    
    (* Mark final states with double circle *)
    if List.mem state automaton.Automaton.final_states then
      Printf.fprintf oc "    style %d stroke-dasharray: 5 5\n" state

  ) automaton.Automaton.states;
  
  (* Add transitions *)
  List.iter (fun (source, symbol, target) ->
    Printf.fprintf oc "    %d -->|%s| %d\n" source symbol target
  ) automaton.Automaton.transitions;
  
  (* Close mermaid diagram *)
  Printf.fprintf oc "```\n\n";
  
  (* Add legend *)
  Printf.fprintf oc "## Legend\n\n";
  Printf.fprintf oc "- Blue state: Initial state\n";
  Printf.fprintf oc "- Dashed state: Final/accepting state\n";
  Printf.fprintf oc "- Solid state: Normal state\n\n";
  
  (* Add automaton details *)
  Printf.fprintf oc "## Automaton Details\n\n";
  Printf.fprintf oc "### Alphabet\n";
  Printf.fprintf oc  "```\n%s\n```\n\n"
    (String.concat ", " automaton.Automaton.alphabet);
  
  Printf.fprintf oc "### States\n";
  Printf.fprintf oc "```\n%s\n```\n\n"
    (String.concat ", " (List.map string_of_int automaton.Automaton.states));

  Printf.fprintf oc "### Initial State\n";
  Printf.fprintf oc "```\n%d\n```\n\n" automaton.Automaton.initial_state;

  Printf.fprintf oc "### Final States\n";
  Printf.fprintf oc "```\n%s\n```\n\n"
    (String.concat ", " (List.map string_of_int automaton.Automaton.final_states));
  
  (* Close the file *)
  close_out oc;
  
  print_endline (Printf.sprintf "Mermaid diagram saved to %s" filepath)

(* Generate visualization and display key mappings for the automaton *)
let draw_automaton automaton filename =
  (* Generate and save a Mermaid diagram of the automaton *)
  let output_file = Filename.remove_extension (Filename.basename filename) ^ ".md" in
  generate_mermaid_diagram automaton output_file;
  (* Print the key mappings for the automaton's alphabet *)
  print_key_mappings automaton.Automaton.alphabet
