(* Reads a grammar file line by line and returns its contents as a list of strings *)
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

(* Processes a user input sequence against the automaton
   Tokenizes the input and checks if it forms a recognizable pattern *)
let process_input_sequence automaton input_sequence =
  let symbols = Parser.tokenize input_sequence in
  let recognized = Automaton.process_input automaton symbols automaton.Automaton.initial_state in
  if recognized then Display.print_recognized_move symbols

(* Main function that runs the application *)
let run () =
  (* Check command line arguments *)
  if Array.length Sys.argv < 2 then (
    print_endline "Usage: ft_ality grammar_file";
    exit 1
  );
  let grammar_file = Sys.argv.(1) in
  
  (* Read and parse the grammar file *)
  let grammar_lines = read_grammar_file grammar_file in
  
  (* Build the automaton from grammar lines *)
  let automaton = List.fold_left (fun acc line ->
    Parser.parse_grammar_line line acc
  ) Automaton.empty grammar_lines in
  
  (* Display available key mappings to the user *)
  Display.print_key_mappings automaton.Automaton.alphabet;
  
  (* Interactive input loop *)
  print_endline "Waiting for input (press Ctrl+D to exit):";
  try
    while true do
      let line = read_line () in
      process_input_sequence automaton line
    done
  with End_of_file -> ()

(* Program entry point *)
let () = run ()
