type symbol = string
type state = int
type transition = state * symbol * state

(* The automaton record type *)
type t = {
  alphabet: symbol list;
  states: state list;
  initial_state: state;
  final_states: state list;
  transitions: transition list;
}

(* Empty automaton *)
val empty : t

(* Add a state to the automaton *)
val add_state : state -> t -> t

(* Add a symbol to the automaton's alphabet *)
val add_symbol : symbol -> t -> t

(* Add a transition to the automaton *)
val add_transition : transition -> t -> t

(* Add a final state to the automaton *)
val add_final_state : state -> t -> t

(* Process input against the automaton *)
val process_input : t -> symbol list -> state -> bool