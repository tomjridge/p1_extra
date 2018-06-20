open P1_core
open Lambda_calc_example

let do_it s = 
  Printf.printf "Parsing string: %s\n%!" s;
  run_parser lambda_calc_parser s
  |> List.iter (fun t -> 
      t 
      |> term_to_yojson
      |> Yojson.Safe.pretty_to_string
      |> print_endline)
  |> fun _ -> print_endline "----------------------------------------"

(* NOTE lots of ambiguity, even in first example *)
let _ = List.iter do_it 
    ["\\ x.x x";
     "\\ x.x x x";
     "\\ x.(x x) x";
     "\\ x. \\y. \\z. (x y) z";
    ]

