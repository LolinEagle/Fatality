(* Helper function that performs the actual validation logic *)
val validate_tokens : string list -> bool

(* Determine if a line should be skipped (empty or comment) *)
val is_skippable_line : string -> bool