(* Helper function that performs the actual validation logic *)
let validate_tokens tokens =
  match tokens with
  | [] -> false (* Empty line is invalid *)
  | _ -> 
      (* Check if there's exactly one token starting with '=' *)
      let equals_tokens = List.filter (fun t -> String.length t > 0 && t.[0] = '=') tokens in
      if List.length equals_tokens <> 1 then false
      else
        let equals_token = List.hd equals_tokens in
        (* Verify equals token has content after '=' character *)
        let has_content_after_equals = String.length equals_token > 1 in
        
        (* Check if there's a non-empty token before equals sign *)
        let rec has_valid_token_before_equals remaining seen_valid_token =
          match remaining with
          | [] -> false
          | t :: rest ->
              if t = equals_token then 
                (* Only return true if we've seen a non-empty token before *)
                seen_valid_token 
              else 
                (* Only count this as a valid token if it's not empty *)
                has_valid_token_before_equals rest (seen_valid_token || t <> "")
        in
        
        has_content_after_equals && has_valid_token_before_equals tokens false

(* Determine if a line should be skipped (empty or comment) *)
let is_skippable_line line =
  let trimmed = String.trim line in
  trimmed = "" || (String.length trimmed > 0 && trimmed.[0] = '#')
