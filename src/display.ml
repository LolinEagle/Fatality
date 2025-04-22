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