(* common terminals *)
open Tjr_substring 
open P1_core

open String_position

let sub ~i ~j s = String.sub s i (j-i)

(* input to string position conversion *)
let i2p : input -> string_position = String_position.(fun i -> {s_=i.ss.s_;i_=i.ss.i_})

(* wrap various Tjr_substring funs *)
let a lit : string parser_ = fun i -> 
  i |> i2p 
  |> a lit 
  |> List.filter (fun j -> j <= i.ss.j_)
  |> (List.map @@ fun i' -> (lit,{i.ss with i_=i' }))

let index_to_string i i' = sub ~i:i.ss.i_ ~j:i' i.ss.s_,{i.ss with i_=i' }

let upto_a lit : string parser_ = fun i -> 
  i |> i2p 
  |> upto_a lit 
  |> List.filter (fun j -> j <= i.ss.j_)
  |> List.map @@ fun i' -> index_to_string i i'

let re re : string parser_ = fun i -> 
  i |> i2p
  |> Tjr_substring.re ~re
  |> List.filter (fun j -> j <= i.ss.j_)
  |> List.map @@ fun i' -> index_to_string i i'

(* NOTE empty string not accepted for following *)
let ws : string parser_ = re Str.(regexp "[ \n]+")
  
let _AZs : string parser_ = re Str.(regexp "[A-Z]+")

let _AZazs = re Str.(regexp "[A-Za-z]+")

let eof : unit parser_ = fun i ->
  if i.ss.i_ = String.length i.ss.s_ then [(),i.ss] else []
