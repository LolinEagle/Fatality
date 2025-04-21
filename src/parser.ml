(* Tokenizes an input line by splitting on whitespace and commas
   Returns a list of tokens in their original order *)
let tokenize line =
  let rec aux acc current = function
    | [] -> 
      (if current <> "" then current :: acc else acc)  (* Add final token if not empty *)
    | ' ' :: rest ->
      aux (if current <> "" then current :: acc else acc) "" rest  (* Space found - finalize current token *)
    | ',' :: rest ->
      aux (if current <> "" then current :: acc else acc) "" rest  (* Comma found - finalize current token *)
    | c :: rest ->
      aux acc (current ^ (String.make 1 c)) rest  (* Add character to current token *)
  in
  List.rev (aux [] "" (List.of_seq (String.to_seq line)))  (* Process char-by-char then reverse to maintain order *)

(* Parses a grammar line and updates the automaton accordingly
   Each grammar line represents a sequence of symbols that should be recognized *)
let parse_grammar_line line automaton =
  let tokens = tokenize line in
  
  (* Recursively builds transitions for a sequence of tokens
     Each token creates a transition to a new state *)
  let rec build_transitions current_state ts automaton =
    match ts with
    | [] -> automaton  (* No more tokens to process *)
    | t :: rest ->
      let new_state = 
        match automaton.Automaton.states with
        | [] -> 1  (* First state after initial *)
        | _ -> List.fold_left max 0 automaton.Automaton.states + 1  (* Generate a new state number *)
      in
      let automaton = Automaton.add_transition (current_state, t, new_state) automaton in
      build_transitions new_state rest automaton  (* Continue with next token *)
  in
  
  (* Set up initial state and build transitions for all tokens *)
  let start_state = 0 in
  let automaton = automaton |> Automaton.set_initial_state start_state in
  let automaton = build_transitions start_state tokens automaton in
  
  (* Mark all states as final states - any complete sequence will be recognized *)
  List.fold_left (fun acc q -> Automaton.add_final_state q acc) automaton automaton.Automaton.states