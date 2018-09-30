(* some common p1 combinators *)

open P1_core

let eps : string parser_ = fun i -> [("",i.ss)]

module Shortcut_alternative = struct

  (* only try p2 if no results from p1 *)
  let ( ||| ) p1 p2 = fun s -> 
    p1 s |> fun xs -> if xs=[] then p2 s else xs

end

(* NOTE in folllowing defns of star and plus there is no check, so p
 **> sep should not parse eps; NOTE eta expansion *)

module Greedy = struct

  open Shortcut_alternative
      
(*
  let rec star ~sep p : 'a list parser_ = fun i -> (
      (p **> sep **> star ~sep p >> fun (x,(y,z)) -> x::z) |||
      (p >> fun x -> [x]) |||
      (eps >> fun _ -> [])  ) i
*)
  let rec greedy_star ~sep p : 'a list parser_ = fun i -> (
      (greedy_plus ~sep p) |||
      (eps >> fun _ -> [])  ) i
  and greedy_plus ~sep p : 'a list parser_ = fun i -> (
    let rest = 
      (sep **> greedy_plus ~sep p >> fun (_,xs) -> xs) |||
      (eps >> fun _ -> [])
    in
    (p **> rest >> fun (x,xs) -> x::xs)  ) i

end

(*
(* alternative defn *)
let rec star ~sep p : 'a list parser_ = fun i -> (
    let rest = 
      (sep **> star ~sep p >> fun (_,z) -> z) |||
      (eps >> fun _ -> [])
    in
    (p **> rest >> fun (x,xs) -> x::xs) |||
    (eps >> fun _ -> [])  ) i

let plus ~sep p : 'a list parser_ = 
  (p **> sep **> star ~sep p >> fun (x,(_,xs)) -> x::xs) |||
  (p >> fun x -> [x]) 
*)

module Match_all_ways = struct

  let rec star ~sep p : 'a list parser_ = fun i -> (
      (plus ~sep p) |||
      (eps >> fun _ -> [])  ) i
  and plus ~sep p : 'a list parser_ = fun i -> (
      let rest = 
        (sep **> plus ~sep p >> fun (_,xs) -> xs) |||
        (eps >> fun _ -> [])
      in
      (p **> rest >> fun (x,xs) -> x::xs)  ) i

end

include Greedy  (* FIXME may want a version which produces all possible matches *)

include Match_all_ways


let opt p : 'a option parser_ = 
  (eps >> fun _ -> None) |||
  (p >> fun x -> Some x)

let rec iter ~n p : 'a list parser_ = 
  match n with
  | 0 -> eps >> fun _ -> []
  | 1 -> p >> fun x -> [x]
  | _ -> p **> (iter ~n:(n-1) p) >> fun (x,xs) -> x::xs

let rec seq_list = 
  let open P1_core in
  function
  | [] -> failwith __LOC__
  | [x] -> x>>fun x -> [x]
  | x::xs -> x **> seq_list xs >> fun (x,xs) -> x::xs

let rec alt_list = 
  let open P1_core in
  function
  | [] -> failwith __LOC__
  | [x] -> x
  | x::xs -> x ||| alt_list xs


let p1_log ~msg p i = 
  let open Tjr_substring in
  Printf.printf "%s: called on (%d,%d)" msg i.ss.i_ i.ss.j_;
  p i
