(* Print the key mappings (alphabet) *)
val print_key_mappings : string list -> unit

(* Print a recognized move *)
val print_recognized_move : Automaton.state -> Automaton.automaton -> unit

(* Generate a mermaid diagram representation of an automaton and save to a markdown file *)
val generate_mermaid_diagram : Automaton.automaton -> string -> unit

(* Generate visualization and display key mappings for the automaton *)
val draw_automaton : Automaton.automaton -> string -> unit
