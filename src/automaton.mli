(* Type definitions *)
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

(* Creates an empty automaton *)
val empty : t

(* Adds a state if not already present *)
val add_state : state -> t -> t

(* Adds a symbol to alphabet if new *)
val add_symbol : symbol -> t -> t

(* Adds a transition while ensuring states and symbol exist *)
val add_transition : transition -> t -> t

(* Updates initial state *)
val set_initial_state : state -> t -> t

(* Adds state to final states if not already present *)
val add_final_state : state -> t -> t

(* Process input string against automaton *)
val process_input : t -> symbol list -> state -> bool
