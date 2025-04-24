(* Build transitions from tokens to create automaton *)
let build_transitions current_state tokens automaton =
  let rec process_tokens current_state ts automaton =
    match ts with
    | [] -> (* Mark final state when combo is complete *)
      Some (Automaton.add_final_state current_state automaton)
    | t :: rest -> (* Check if transition already exists *)
      if String.length t > 0 && t.[0] = '=' then
        let description = String.sub t 1 (String.length t - 1) in
        let automaton = Automaton.add_combo (current_state, description) automaton in
        Some (Automaton.add_final_state current_state automaton)
      else
        let existing = List.find_opt (fun (q, sym, _) -> q = current_state && sym = t) automaton.Automaton.transitions in
        match existing with
        | Some (_, _, next_state) -> (* Use existing transition *)
          process_tokens next_state rest automaton
        | None -> (* Create new transition *)   
          let new_state = 
            match automaton.Automaton.states with
            (* If no states exist starts at 1, otherwise takes max state + 1 *)
            | [] -> 1
            | _ -> List.fold_left max 0 automaton.Automaton.states + 1 
          in
          let automaton = Automaton.add_transition (current_state, t, new_state) automaton in
          process_tokens new_state rest automaton (* Recursively processes *)
  in
  process_tokens current_state tokens automaton

(* Validate and build automaton from tokens *)
let validate_tokens_and_build tokens automaton =
  if not (Validate.validate_tokens tokens) then 
    None
  else
    build_transitions automaton.Automaton.initial_state tokens automaton

(* Build automaton from a grammar file, line by line *)
let build_automaton_from_file grammar_file =
  (* Open the file for reading *)
  let ic = 
    try open_in grammar_file
    with Sys_error msg -> 
      Printf.fprintf stderr "Error opening file: %s\n" msg;
      exit 1
  in
  
  (* Track seen combos (sequence of tokens) to detect duplicates *)
  let seen_combos = Hashtbl.create 50 in
  
  (* Process each line as we read it, starting with an empty automaton *)
  let rec process_lines automaton line_num =
    try
      let line = input_line ic in
      let trimmed_line = String.trim line in
      
      (* Skip empty lines and comments, otherwise validate and parse *)
      let new_automaton = 
        if Validate.is_skippable_line line then
          automaton (* Return unchanged automaton *)
        else
          let tokens = Parser.tokenize trimmed_line in
          
          (* Extract sequence (combo) and action *)
          let combo_tokens = List.filter (fun t -> String.length t = 0 || t.[0] <> '=') tokens in
          
          (* Check for duplicate combos *)
          let combo_key = String.concat "," combo_tokens in
          if Hashtbl.mem seen_combos combo_key then begin
            Printf.fprintf stderr "Error: Duplicate combo '%s' at line %d in %s\n" 
              combo_key line_num grammar_file;
            Printf.fprintf stderr "This combo was already defined earlier in the file\n";
            close_in ic;
            exit 1
          end;
          
          (* Record combo if not a duplicate *)
          Hashtbl.add seen_combos combo_key true;
          
          (* Continue with token validation and build *)
          match validate_tokens_and_build tokens automaton with
          | Some new_automaton -> new_automaton
          | None ->
              begin
                Printf.fprintf stderr "Error: Invalid grammar line format at line %d in %s\n" line_num grammar_file;
                Printf.fprintf stderr "Grammar lines must have the format: 'input1,input2,...=ComboName'\n";
                close_in ic;
                exit 1
              end
      in
      process_lines new_automaton (line_num + 1)
    with
    | End_of_file -> 
        close_in ic;
        automaton
    | exn ->
        close_in ic;
        Printf.fprintf stderr "Error while reading file: %s\n" (Printexc.to_string exn);
        exit 1
  in
  
  let automaton = process_lines Automaton.empty 1 in
  
  (* Verify the automaton is not empty *)
  if automaton.Automaton.states = [] then begin
    Printf.fprintf stderr "Error: No valid grammar rules found in %s\n" grammar_file;
    exit 1
  end;
  
  automaton
