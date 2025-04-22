(* Read grammar rules from a file *)
val read_grammar_file : string -> string list

(* Process an input sequence against an automaton *)
val process_input_sequence : Automaton.automaton -> string -> unit

(* Generate and save a Mermaid diagram of the automaton *)
val draw : Automaton.automaton -> unit

(* Run an interactive game mode to test the automaton *)
val game : Automaton.automaton -> unit

(* Build an automaton from grammar lines *)
val build_automaton : string list -> Automaton.automaton
