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

let empty = {
  alphabet = [];
  states = [];
  initial_state = 0;
  final_states = [];
  transitions = [];
}

let add_state q automaton =
  (* Adds a state if not already present (immutable update) *)
  if List.mem q automaton.states then automaton
  else { automaton with states = q :: automaton.states }

let add_symbol s automaton =
  (* Adds a symbol to alphabet if new *)
  if List.mem s automaton.alphabet then automaton
  else { automaton with alphabet = s :: automaton.alphabet }

let add_transition (q1, sym, q2) automaton =
  (* Adds a transition while ensuring: *)
  let automaton = add_state q1 automaton in   (* 1 Source state exists *)
  let automaton = add_state q2 automaton in   (* 2 Destination state exists *)
  let automaton = add_symbol sym automaton in (* 3 Symbol is in alphabet *)
  (* 4 Adds the new transition *)
  { automaton with transitions = (q1, sym, q2) :: automaton.transitions }

let add_final_state q automaton =
  (* Adds state to final states if not already present *)
  if List.mem q automaton.final_states then automaton
  else { automaton with final_states = q :: automaton.final_states }

(* Recursive function to process input symbols against automaton *)
let rec process_input automaton input current_state =
  match input with
  (* Base case: if no input left, accept if current state is final *)
  | [] -> List.mem current_state automaton.final_states
  (* For each input symbol: *)
  | sym :: rest ->
    (* 1 Looks for valid transition from current state *)
    match List.find_opt (fun (q1, s, _) -> q1 = current_state && s = sym) automaton.transitions with
    (* 2 If found, continues with next state *)
    | Some (_, _, q2) -> process_input automaton rest q2
    (* 3 If not found, rejects input *)
    | None -> false