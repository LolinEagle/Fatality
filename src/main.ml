module Automaton = struct
  type symbol = string
  type state = int
  type transition = state * symbol * state
  
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
    if List.mem q automaton.states then automaton
    else { automaton with states = q :: automaton.states }

  let add_symbol s automaton =
    if List.mem s automaton.alphabet then automaton
    else { automaton with alphabet = s :: automaton.alphabet }

  let add_transition (q1, sym, q2) automaton =
    let automaton = add_state q1 automaton in
    let automaton = add_state q2 automaton in
    let automaton = add_symbol sym automaton in
    { automaton with transitions = (q1, sym, q2) :: automaton.transitions }

  let set_initial_state q automaton =
    { automaton with initial_state = q }

  let add_final_state q automaton =
    if List.mem q automaton.final_states then automaton
    else { automaton with final_states = q :: automaton.final_states }

  let rec process_input automaton input current_state =
    match input with
    | [] -> List.mem current_state automaton.final_states
    | sym :: rest ->
      match List.find_opt (fun (q1, s, _) -> q1 = current_state && s = sym) automaton.transitions with
      | Some (_, _, q2) -> process_input automaton rest q2
      | None -> false
end

module GrammarParser = struct
  let tokenize line =
    let rec aux acc current = function
      | [] -> 
        (if current <> "" then current :: acc else acc)
      | ' ' :: rest ->
        aux (if current <> "" then current :: acc else acc) "" rest
      | ',' :: rest ->
        aux (if current <> "" then current :: acc else acc) "" rest
      | c :: rest ->
        aux acc (current ^ (String.make 1 c)) rest
    in
    List.rev (aux [] "" (List.of_seq (String.to_seq line)))

  let parse_grammar_line line automaton =
    let tokens = tokenize line in
    let rec build_transitions current_state ts automaton =
      match ts with
      | [] -> automaton
      | t :: rest ->
        let new_state = 
          match automaton.Automaton.states with
          | [] -> 1
          | _ -> List.fold_left max 0 automaton.Automaton.states + 1
        in
        let automaton = Automaton.add_transition (current_state, t, new_state) automaton in
        build_transitions new_state rest automaton
    in
    let start_state = 0 in
    let automaton = automaton |> Automaton.set_initial_state start_state in
    let automaton = build_transitions start_state tokens automaton in
    List.fold_left (fun acc q -> Automaton.add_final_state q acc) automaton automaton.Automaton.states
end

module Display = struct
  let print_key_mappings alphabet =
    print_endline "Key mappings:";
    List.iter (fun key ->
      print_endline (Printf.sprintf "%s -> [%s]" key key)
    ) alphabet;
    print_endline "----------------------"

  let print_recognized_move move =
    print_endline (Printf.sprintf "%s recognized!!" (String.concat " " move))
end

module Main = struct
  let read_grammar_file filename =
    let ic = open_in filename in
    let rec read_lines acc =
      try
        let line = input_line ic in
        read_lines (line :: acc)
      with End_of_file -> 
        close_in ic;
        List.rev acc
    in
    read_lines []

  let process_input_sequence automaton input_sequence =
    let symbols = GrammarParser.tokenize input_sequence in
    let recognized = Automaton.process_input automaton symbols automaton.Automaton.initial_state in
    if recognized then Display.print_recognized_move symbols

  let run () =
    if Array.length Sys.argv < 2 then (
      print_endline "Usage: ./ft_ality grammar_file";
      exit 1
    );
    let grammar_file = Sys.argv.(1) in
    let grammar_lines = read_grammar_file grammar_file in
    
    let automaton = List.fold_left (fun acc line ->
      GrammarParser.parse_grammar_line line acc
    ) Automaton.empty grammar_lines in
    
    Display.print_key_mappings automaton.Automaton.alphabet;
    
    print_endline "Waiting for input (press Ctrl+D to exit):";
    try
      while true do
        let line = read_line () in
        process_input_sequence automaton line
      done
    with End_of_file -> ()
end

let () = Main.run ()
