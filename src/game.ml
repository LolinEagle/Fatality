(* Process a single input sequence against the automaton *)
let process_input automaton input =
  let symbols = Parser.tokenize input in
  (* Processes the tokens, starting from its initial state *)
  let recognized = Automaton.process_input automaton symbols automaton.Automaton.initial_state in
  if recognized > 0 then Display.print_recognized_move recognized automaton

(* Main game loop that waits for user input *)
let game automaton =
  print_endline "Waiting for input (press Ctrl+D to exit):";
  try
    while true do
      let line = read_line () in
      process_input automaton line
    done
  with End_of_file -> ()