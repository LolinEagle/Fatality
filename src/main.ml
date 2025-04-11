module Automaton = struct
  type symbol = string                    (* representing automaton alphabet *)
  type state = int                        (* Integer representing states *)
  type transition = state * symbol * state(* Tuple *)
  
  (* The automaton record type *)
  type t = {
    alphabet: symbol list;        (* Current alphabet symbols *)
    states: state list;           (* All states *)
    initial_state: state;         (* Initial state *)
    final_states: state list;     (* Final (accepting) states *)
    transitions: transition list; (* All transitions *)
  }

  (* Creates an empty automaton *)
  let empty = {
    alphabet = [];
    states = [];
    initial_state = 0;
    final_states = [];
    transitions = [];
  }

  (* Adds a state if not already present (immutable update) *)
  let add_state q automaton =
    if List.mem q automaton.states then automaton
    else { automaton with states = q :: automaton.states }

  (* Adds a symbol to alphabet if new *)
  let add_symbol s automaton =
    if List.mem s automaton.alphabet then automaton
    else { automaton with alphabet = s :: automaton.alphabet }

  (* Adds a transition while ensuring: *)
  let add_transition (q1, sym, q2) automaton =
    let automaton = add_state q1 automaton in   (* 1 Source state exists *)
    let automaton = add_state q2 automaton in   (* 2 Destination state exists *)
    let automaton = add_symbol sym automaton in (* 3 Symbol is in alphabet *)
    (* 4 Adds the new transition *)
    { automaton with transitions = (q1, sym, q2) :: automaton.transitions }

  (* Updates initial state (shallow copy) *)
  let set_initial_state q automaton =
    { automaton with initial_state = q }

  (* Adds state to final states if not already present *)
  let add_final_state q automaton =
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
end

module GrammarParser = struct
  let tokenize line =
    (* Internal recursive helper function *)
    let rec aux acc current = function
      (* When no characters left, add any pending token to accumulator *)
      | [] -> 
        (if current <> "" then current :: acc else acc)
      (* Finalize current token (if exists) and start new empty token *)
      | ' ' :: rest ->
        aux (if current <> "" then current :: acc else acc) "" rest
      (* Treats commas as separators *)
      | ',' :: rest ->
        aux (if current <> "" then current :: acc else acc) "" rest
      (* Append to current token *)
      | c :: rest ->
        aux acc (current ^ (String.make 1 c)) rest
    in
    (* Converts string to character list, processes it, reverses result *)
    List.rev (aux [] "" (List.of_seq (String.to_seq line)))

  let parse_grammar_line line automaton =
    let tokens = tokenize line in
    (* Internal recursive function to build state transitions from tokens *)
    let rec build_transitions current_state ts automaton =
      match ts with
      (* Return automaton when no tokens left *)
      | [] -> automaton
      (* Determines next state number: *)
      | t :: rest ->
        let new_state = 
          match automaton.Automaton.states with
          (* If no states exist, starts at 1 *)
          | [] -> 1
          (* Otherwise takes max state + 1 *)
          | _ -> List.fold_left max 0 automaton.Automaton.states + 1
        in
        (* Adds transition (current_state --token--> new_state) *)
        let automaton = Automaton.add_transition (current_state, t, new_state) automaton in
        (* Recursively processes remaining tokens *)
        build_transitions new_state rest automaton
    in
    let start_state = 0 in
    (* Sets automaton's initial state *)
    let automaton = automaton |> Automaton.set_initial_state start_state in
    (* Builds transitions from the start state *)
    let automaton = build_transitions start_state tokens automaton in
    (* Marks all states as final (folding through states list) *)
    List.fold_left (fun acc q -> Automaton.add_final_state q acc) automaton automaton.Automaton.states
end

module Display = struct
  let print_key_mappings alphabet =
    print_endline "Key mappings:";
    (* Iterating over each element in the alphabet list *)
    List.iter (fun key ->
      (* For each key, prints a formatted line showing the mapping *)
      print_endline (Printf.sprintf "%s" key)
    ) alphabet;
    print_endline "____________________________________________________________"

  let print_recognized_move move =
    print_endline (Printf.sprintf "%s recognized" (String.concat " " move))
end

module Main = struct
  let read_grammar_file filename =
    let ic = open_in filename in
    (* Recursive function that takes an accumulator *)
    let rec read_lines acc =
      try
        let line = input_line ic in (* Tries to read a line from the input *)
        read_lines (line :: acc)    (* Calls read_lines with the new line *)
      with End_of_file -> 
        close_in ic;                (* If the end of file is reached, closes *)
        List.rev acc                (* Returns the accumulated lines *)
    in
    read_lines [] (* Starts the recursive reading with an empty accumulator *)

  let process_input_sequence automaton input_sequence =
    (* Tokenizes the input sequence into symbols *)
    let symbols = GrammarParser.tokenize input_sequence in
    (* Processes the tokens, starting from its initial state *)
    let recognized = Automaton.process_input automaton symbols automaton.Automaton.initial_state in
    if recognized then Display.print_recognized_move symbols

  let run () =
    (* Checks if a grammar file was provided as command line argument *)
    if Array.length Sys.argv < 2 then (
      print_endline "Usage: ft_ality grammar_file";
      exit 1
    );
    let grammar_file = Sys.argv.(1) in
    let grammar_lines = read_grammar_file grammar_file in
    
    (* Builds an automaton by parsing each grammar line *)
    let automaton = List.fold_left (fun acc line ->
      GrammarParser.parse_grammar_line line acc
    ) Automaton.empty grammar_lines in
    
    (* Prints the key mappings from the automaton's alphabet *)
    Display.print_key_mappings automaton.Automaton.alphabet;
    
    print_endline "Waiting for input (press Ctrl+D to exit):";
    (* Prints a prompt message *)
    try
      while true do
        let line = read_line () in
        process_input_sequence automaton line
      done
    with End_of_file -> ()  (* Catches the End_of_file exception to exit *)
end

let () = Main.run ()
