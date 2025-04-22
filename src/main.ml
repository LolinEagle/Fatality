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
  if recognized then print_endline (Printf.sprintf "%s recognized" (String.concat " " symbols))

let draw automaton =
  (* Generate and save a Mermaid diagram of the automaton *)
  let output_file = Filename.remove_extension (Filename.basename Sys.argv.(1)) ^ ".md" in
  Display.generate_mermaid_diagram automaton output_file

let game automaton =
  (* Main game loop that waits for user input *)
  print_endline "Waiting for input (press Ctrl+D to exit):";
  try
    while true do
      let line = read_line () in
      process_input_sequence automaton line
    done
  with End_of_file -> ()

let build_automaton grammar_lines =
  (* Builds an automaton by parsing each grammar line *)
  List.fold_left (fun acc line ->
    Parser.parse_grammar_line line acc
  ) Automaton.empty grammar_lines

let () =
  match Sys.argv with
  | [| _; grammar_file |] ->
      let grammar_lines = read_grammar_file grammar_file in
      let automaton = build_automaton grammar_lines in
      draw automaton;
      game automaton
  | _ ->
      print_endline "Usage: ft_ality grammar_file";
      exit 1
