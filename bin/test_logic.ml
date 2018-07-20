open P1_core
open Logic

let do_it s = 
  Printf.printf "Parsing string: %s\n%!" s;
  run_parser logic_parser s
  |> List.iter (fun t -> 
      t 
      |> form_to_yojson
      |> Yojson.Safe.pretty_to_string
      |> print_endline)
  |> fun _ -> print_endline "----------------------------------------"

let _ = List.iter do_it 
    ["! x. `P x`";
     {|`A` /\ `B`|};
     {|(`A` /\ `B`)|};
     {|(`A` --> `B`)|};
     (* {|? x. (`A x` /\ `B`) --> (`B` /\ `A x`)|} *)
    ]

(* NOTE p1 parsing is very slow ! could fix this by taking advantage
   of the bracket structure: parsing a bracket should be a terminal
   which invokes logic parser on the span delimited by the brackets;
   of course, earley parsing would not have the same issues *)
