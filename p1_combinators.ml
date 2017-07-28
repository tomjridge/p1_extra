(* some common p1 combinators *)

open P1_core

let eps : string parser_ = fun i -> [("",i.ss)]

(* no check, so p **> sep should not parse eps; NOTE eta expansion *)
let rec star ~sep p : 'a list parser_ = fun i -> (
    (eps >> fun _ -> []) |||
    (p >> fun x -> [x]) |||
    (p **> sep **> star ~sep p >> fun (x,(y,z)) -> x::z)) i

let plus ~sep p : 'a list parser_ = 
  (p >> fun x -> [x]) |||
  (p **> sep **> star ~sep p >> fun (x,(_,xs)) -> x::xs)

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

