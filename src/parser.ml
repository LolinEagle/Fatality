let tokenize line =
  let rec aux acc current = function
    | [] -> (* When no characters left, add any pending token to accumulator *)
      (if current <> "" then current :: acc else acc)
    | '=' :: rest ->  (* When we hit =, make the rest a single token *)
      let rest_str = String.of_seq (List.to_seq ('=' :: rest)) in
      (rest_str :: current :: acc)
    | ' ' :: rest -> (* Finalize current token and start new empty token *)
      aux (if current <> "" then current :: acc else acc) "" rest
    | ',' :: rest -> (* Treats commas as separators *)
      aux (if current <> "" then current :: acc else acc) "" rest
    | c :: rest -> (* Append to current token *)
      aux acc (current ^ (String.make 1 c)) rest
  in
  (* Converts string to character list, processes it, reverses result *)
  List.rev (aux [] "" (List.of_seq (String.to_seq line)))
