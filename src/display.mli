(* Interface for the Display module *)

(* Print the key mappings (alphabet) *)
val print_key_mappings : string list -> unit

(* Print a recognized move *)
val print_recognized_move : Automaton.state -> Automaton.t -> unit
