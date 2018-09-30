 
(* NOTE this is a generated file; do not edit. See test.ml for source. *)

open P1_core

let mk_parser ~ws ~eof ~a = 
let rec 
_S i = check "S" (
  (_E **> ws **> eof >> (fun (x1,(x2,x3)) ->  
  print_endline (x1 |> string_of_int) ) )) i and 

_E i = check "E" (
  (_E **> _E **> _E >> (fun (x1,(x2,x3)) ->  x1+x2+x3 ) )|||
  (a "1" >> (fun x1 ->  1 ) )|||
  (a "" >> (fun x1 ->  0 ) )) i
in _S

  