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
  let symbols = Parser.tokenize input_sequence in
  (* Processes the tokens, starting from its initial state *)
  let recognized = Automaton.process_input automaton symbols automaton.Automaton.initial_state in
  if recognized > 0 then Display.print_recognized_move recognized automaton

let run () =
  (* Checks if a grammar file was provided as command line argument *)
  if Array.length Sys.argv <> 2 then (
    print_endline "Usage: ft_ality grammar_file";
    exit 1
  );
  let grammar_lines = read_grammar_file Sys.argv.(1) in
  
  (* Builds an automaton by parsing each grammar line *)
  let automaton = List.fold_left (fun acc line ->
    Parser.parse_grammar_line line acc
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
  with End_of_file -> () (* Catches the End_of_file exception to exit *)

let () = run ()
