type symbol = string
type state = int
type transition = state * symbol * state

(* The automaton record type *)
type automaton = {
  alphabet: symbol list;
  states: state list;
  initial_state: state;
  final_states: state list;
  transitions: transition list;
}

(* Empty automaton *)
val empty : automaton

(* Add a state to the automaton *)
val add_state : state -> automaton -> automaton

(* Add a symbol to the automaton's alphabet *)
val add_symbol : symbol -> automaton -> automaton

(* Add a transition to the automaton *)
val add_transition : transition -> automaton -> automaton

(* Add a final state to the automaton *)
val add_final_state : state -> automaton -> automaton

(* Process input against the automaton *)
val process_input : automaton -> symbol list -> state -> bool