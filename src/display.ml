let generate_mermaid_diagram automaton filename =
  (* Ensure automaton directory exists *)
  let automaton_dir = "automaton" in
  if not (Sys.file_exists automaton_dir) then
    Sys.mkdir automaton_dir 0o755;
  
  (* Create or open file for writing in the automaton directory *)
  let filepath = Filename.concat automaton_dir filename in
  let oc = open_out filepath in
  
  (* Write markdown header *)
  output_string oc "# Automaton Visualization\n\n";
  output_string oc "```mermaid\nflowchart LR\n";
  
  (* Style for states *)
  List.iter (fun state ->
    (* Regular state *)
    output_string oc (Printf.sprintf "    %d([%d])\n" state state);
    
    (* Mark initial state with different style *)
    if state = automaton.Automaton.initial_state then
      output_string oc (Printf.sprintf "    style %d fill:#8af,stroke:#333,stroke-width:2px\n" state);
    
    (* Mark final states with double circle *)
    if List.mem state automaton.Automaton.final_states then
      output_string oc (Printf.sprintf "    style %d stroke-dasharray: 5 5\n" state)
  ) automaton.Automaton.states;
  
  (* Add transitions *)
  List.iter (fun (source, symbol, target) ->
    output_string oc (Printf.sprintf "    %d -->|%s| %d\n" source symbol target)
  ) automaton.Automaton.transitions;
  
  (* Close mermaid diagram *)
  output_string oc "```\n\n";
  
  (* Add legend *)
  output_string oc "## Legend\n\n";
  output_string oc "- Blue state: Initial state\n";
  output_string oc "- Dashed state: Final/accepting state\n";
  output_string oc "- Solid state: Normal state\n\n";
  
  (* Add automaton details *)
  output_string oc "## Automaton Details\n\n";
  output_string oc "### Alphabet\n";
  output_string oc (Printf.sprintf "```\n%s\n```\n\n" 
    (String.concat ", " automaton.Automaton.alphabet));
  
  output_string oc "### States\n";
  output_string oc (Printf.sprintf "```\n%s\n```\n\n" 
    (String.concat ", " (List.map string_of_int automaton.Automaton.states)));
  
  output_string oc "### Initial State\n";
  output_string oc (Printf.sprintf "```\n%d\n```\n\n" automaton.Automaton.initial_state);
  
  output_string oc "### Final States\n";
  output_string oc (Printf.sprintf "```\n%s\n```\n\n" 
    (String.concat ", " (List.map string_of_int automaton.Automaton.final_states)));
  
  (* Close the file *)
  close_out oc;
  
  print_endline (Printf.sprintf "Mermaid diagram saved to %s" filepath)