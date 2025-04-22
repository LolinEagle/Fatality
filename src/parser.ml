let tokenize line =
  let rec aux acc current = function
    | [] -> (* When no characters left, add any pending token to accumulator *)
      (if current <> "" then current :: acc else acc)
    | ' ' :: rest -> (* Finalize current token and start new empty token *)
      aux (if current <> "" then current :: acc else acc) "" rest
    | ',' :: rest -> (* Treats commas as separators *)
      aux (if current <> "" then current :: acc else acc) "" rest
    | c :: rest -> (* Append to current token *)
      aux acc (current ^ (String.make 1 c)) rest
  in
  (* Converts string to character list, processes it, reverses result *)
  List.rev (aux [] "" (List.of_seq (String.to_seq line)))

let parse_grammar_line line automaton =
  let tokens = tokenize line in
  (* Internal recursive function to build state transitions from tokens *)
  (* current_state: where we're transitioning from *)
  (* ts: remaining tokens to process *)
  (* automaton: the automaton being constructed *)
  let rec build_transitions current_state ts automaton =
    match ts with
    | [] -> (* Mark final state when combo is complete *)
      Automaton.add_final_state current_state automaton
    | t :: rest -> (* Check if transition already exists *)
      let existing = List.find_opt (fun (q, sym, _) -> q = current_state && sym = t) automaton.Automaton.transitions in
      match existing with
      | Some (_, _, next_state) -> (* Use existing transition *)
        build_transitions next_state rest automaton
      | None -> (* Create new transition *)   
        let new_state = 
          match automaton.Automaton.states with
          (* If no states exist starts at 1, otherwise takes max state + 1 *)
          | [] -> 1
          | _ -> List.fold_left max 0 automaton.Automaton.states + 1 
        in
        let automaton = Automaton.add_transition (current_state, t, new_state) automaton in
        build_transitions new_state rest automaton (* Recursively processes *)
  in
  build_transitions automaton.Automaton.initial_state tokens automaton
