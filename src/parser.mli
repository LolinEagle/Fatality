(* Tokenize a line of input into a list of strings *)
val tokenize : string -> string list

(* Parse a grammar line and update the automaton *)
val parse_grammar_line : string -> Automaton.t -> Automaton.t