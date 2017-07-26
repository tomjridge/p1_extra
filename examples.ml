(* examples --------------------------------------------------------- *)

(* NOTE use an interactive top-level! *)
#require "p1_extra";;

open Tjr_substring
open P1_combinators 

(* for testing FIXME use Tjr_substring.a *)
let a1 (s:substring_) = (
  if s.i_ < s.j_ && s.s_.[s.i_] = '1' then
    [("1",{s with i_ = s.i_ + 1})]
  else
    [])

open P1_core

let a1 : string parser_ = (fun i -> a1 i.s)

let _ = "11111" |> run_parser (iter ~n:5 a1)
let _ = "1111" |> run_parser (iter ~n:5 a1)
let _ = "111111" |> run_parser (iter ~n:5 a1)
