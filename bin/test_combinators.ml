
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
