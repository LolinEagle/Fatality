(* Process a single input sequence against the automaton *)
val process_input : Automaton.automaton -> string -> unit

(* Run an interactive game mode to test the automaton *)
val game : Automaton.automaton -> unit