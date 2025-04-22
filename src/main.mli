(* Interface for the Main module *)

(* Read grammar rules from a file *)
val read_grammar_file : string -> string list

(* Process an input sequence against an automaton *)
val process_input_sequence : Automaton.t -> string -> unit

(* Main entry point for the application *)
val run : unit -> unit