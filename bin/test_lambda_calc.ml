open Lambda_calc_example

let r = 
  "\\ x . ( x x )" 
  |> Tjr_substring.mk_substring
  |> P1_core.to_input
  |> lambda_calc_parser 
  |> List.iter (fun (t,_) -> 
      t 
      |> term_to_yojson
      |> Yojson.Safe.pretty_to_string
      |> print_endline)
