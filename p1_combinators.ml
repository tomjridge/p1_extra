(* some common p1 combinators *)

open P1_core

let eps : string parser_ = fun i -> [("",i.s)]

(* no check, so p **> sep should not parse eps *)
let rec star ~sep p : 'a list parser_ = 
  (eps >> fun _ -> []) |||
  (p >> fun x -> [x]) |||
  (p **> sep **> star ~sep p >> fun (x,(y,z)) -> x::z)

let opt p : 'a option parser_ = 
  (eps >> fun _ -> None) |||
  (p >> fun x -> Some x)

let rec iter ~n p : 'a list parser_ = 
  match n with
  | 0 -> eps >> fun _ -> []
  | 1 -> p >> fun x -> [x]
  | _ -> p **> (iter ~n:(n-1) p) >> fun (x,xs) -> x::xs
