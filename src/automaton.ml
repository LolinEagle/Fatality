(* Basic type definitions for the automaton components *)
type symbol = string
type state = int
type transition = state * symbol * state
  
(* Main automaton record type containing all components of the FSA *)
type t = {
  alphabet: symbol list;      (* List of valid symbols/characters *)
  states: state list;         (* Set of states in the automaton *)
  initial_state: state;       (* Starting state of the automaton *)
  final_states: state list;   (* States that indicate successful recognition *)
  transitions: transition list; (* State transitions based on input symbols *)
}

(* Creates an empty automaton with default values *)
let empty = {
  alphabet = [];
  states = [];
  initial_state = 0;
  final_states = [];
  transitions = [];
}

(* Adds a state to the automaton if not already present *)
let add_state q automaton =
  if List.mem q automaton.states then automaton
  else { automaton with states = q :: automaton.states }

(* Adds a symbol to the alphabet if not already present *)
let add_symbol s automaton =
  if List.mem s automaton.alphabet then automaton
  else { automaton with alphabet = s :: automaton.alphabet }

(* Adds a transition to the automaton, ensuring that states and symbols exist *)
let add_transition (q1, sym, q2) automaton =
  let automaton = add_state q1 automaton in
  let automaton = add_state q2 automaton in
  let automaton = add_symbol sym automaton in
  { automaton with transitions = (q1, sym, q2) :: automaton.transitions }

(* Sets the initial state of the automaton *)
let set_initial_state q automaton =
  { automaton with initial_state = q }

(* Adds a state to the set of final/accepting states *)
let add_final_state q automaton =
  if List.mem q automaton.final_states then automaton
  else { automaton with final_states = q :: automaton.final_states }

(* Processes an input sequence against the automaton to determine if it's accepted
   Returns true if the input leads to a final state, false otherwise *)
let rec process_input automaton input current_state =
  match input with
  | [] -> List.mem current_state automaton.final_states  (* Input consumed - check if we're in a final state *)
  | sym :: rest ->
    match List.find_opt (fun (q1, s, _) -> q1 = current_state && s = sym) automaton.transitions with
    | Some (_, _, q2) -> process_input automaton rest q2  (* Transition found, move to next state *)
    | None -> false  (* No valid transition found, reject input *)