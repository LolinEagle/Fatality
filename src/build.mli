(* Build transitions from tokens to create automaton *)
val build_transitions : int -> string list -> Automaton.automaton -> Automaton.automaton option

(* Validate and build automaton from tokens *)
val validate_tokens_and_build : string list -> Automaton.automaton -> Automaton.automaton option

(* Build automaton from a grammar file, line by line *)
val build_automaton_from_file : string -> Automaton.automaton
