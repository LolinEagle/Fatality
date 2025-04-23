let () =
  match Sys.argv with
  | [| _; grammar_file |] ->
      let automaton = Build.build_automaton_from_file grammar_file in
      Display.draw_automaton automaton grammar_file;
      Game.game automaton
  | _ ->
      print_endline "Usage: ft_ality grammar_file";
      exit 1
