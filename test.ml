
(* examples --------------------------------------------------------- *)

open P1_combinators 

(* for testing FIXME use Tjr_substring.a *)
let a1 (s:Tjr_substring.substring_) = (
  let open Tjr_substring in
  if s.i_ < s.j_ && s.s_.[s.i_] = '1' then
    [("1",{s with i_ = s.i_ + 1})]
  else
    [])

open P1_core

let a1 : string parser_ = (fun i -> a1 i.ss)

let _ = "11111" |> run_parser (iter ~n:5 a1)
let _ = "1111" |> run_parser (iter ~n:5 a1)
let _ = "111111" |> run_parser (iter ~n:5 a1)

;;
let _ = "1111" |> run_parser (star ~sep:eps a1)

open P1_terminals

let a1 = a "1"
let _ = "11111" |> run_parser (iter ~n:5 a1)


let rec _E i = (
  check "E" (
    ((_E **> _E **> _E) >> fun (x,(y,z)) -> x+y+z)
    ||| (a1 >> fun _ -> 1)
    ||| (eps >> fun _ -> 0)) i)

let _ = "111" |> run_parser _E


(* parse_grammar_file ----------------------------------------------- *)

open Parse_grammar_file

let _ = grammar_to_parser

let example = {|

S -> e=E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}

let rs = parse_grammar_file example


(* code gen --------------------------------------------------------- *)

let cg_sym = String.(function
  | `Nt s -> "_"^s  
  (* NOTE prefix with an underscore since we need nts to be func names *)
  | `Qu s -> s
  | `Sq s -> "a "^dq^(escaped s)^dq 
  | `Dq s -> "a "^dq^(escaped s)^dq
  )

let cg_elt = function `Elt (_,sym) -> cg_sym sym

let _fun n = 
  begin
    match n with
    | 1 -> ["x1"]
    | 2 -> ["x1";"x2"]
    | 3 -> ["x1";"x2";"x3"]
    | 4 -> ["x1";"x2";"x3";"x4"]
    | _ -> failwith __LOC__
  end
  |> fun vs ->
  let rec pp = function 
    | [v] -> v
    | [v1;v2] -> "("^v1^","^v2^")"
    | v::vs -> "("^v^","^pp vs^")"
  in
  "fun "^pp vs^" -> "

let cg_rule r =
  (fun (es,code) -> 
     es |> List.map cg_elt |> Tjr_string.concat_strings ~sep:" **> " 
     |> fun es' ->
     "("^es'^" >> ("^_fun (List.length es)^code^") )")
  |> fun f ->
  let open Improved_typing in
  r.rhs 
  |> List.map f 
  |> Tjr_string.concat_strings ~sep:"|||\n  "
  |> fun rhs -> 
  "check "^dq^r.nt^dq^" (\n  "^rhs^")"
  |> fun rhs -> 
  cg_sym (`Nt r.nt) ^ " i = "^rhs^" i"    

let cg_rules rs = 
  rs 
  |> List.map cg_rule 
  |> Tjr_string.concat_strings ~sep:" and \n\n" 
  |> fun body ->
  "let rec \n"^ body


(* test code gen ---------------------------------------------------- *)

let example = {|

S -> e=E ?ws? ?eof? {{ 
  print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x1+x2+x3 }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}

let rs = parse_grammar_file example

(* generated code and output to file *)

let fn = "generated_example.ml"

let _ = 
  cg_rules rs 
  |> fun body ->
  {| 
(* NOTE this is a generated file; do not edit. See test.ml for source. *)

open P1_core

let mk_parser ~ws ~eof ~a = 
$body
in _S

  |} 
  |> Tjr_string.replace_first ~sub:"$body" ~rep:body
  |> Tjr_file.write_string_to_file ~fn

let _ = print_endline @@ "Wrote generated parser to file "^fn 

open P1_terminals

let _S = P1_terminals.(Generated_example.mk_parser ~ws ~eof ~a)

(* need to check of course *)
let _ = "111 " |> run_parser _S
