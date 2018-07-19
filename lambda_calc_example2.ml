(*
(* lambda calc grammar ---------------------------------------------- *)

(* NOTE uses parsing_dsl2 *)

open P1_core
open Parsing_dsl2
open Nt

module X = struct
  type carrier = int
  let c2s c = string_of_int c
  type 'a e_other
  type eo_ops = {
    eo2p: 'a. 'a e_other -> 'a parser_
  }
end

module Dsl2 = Make(X)
open Dsl2
open Rule_ops

let eo_ops = X.{eo2p=fun eo -> failwith "impossible: no other elts"}

open Grammar_requires
open Elt.Terminals



type term = 
    Lam of string * term | App of term * term | Var of string
      [@@deriving yojson]


let lambda_calc_grammar =
  let _TERM, _LAM, _APP, _VAR, _S = mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt() in
  let __ = re "[ \n]*" in
  let rules _X = 
    let x = nt2c _X in
    match () with

    | _ when x = nt2c _S -> [rhs2  (nt _TERM, eof)  (fun (t,_) -> t)]

    (* NOTE this grammar has left recursion (app), and is highly
       ambiguous *)
    (* t = \\ x. t | t1 t2 | x | (t) *)

    | _ when x=nt2c _TERM -> [
        rhs1 (nt _LAM)  (fun t -> t);
        rhs1 (nt _APP)  (fun t -> t);
        rhs1 (nt _VAR)  (fun t -> t);
        rhs5 
          (a "(", __, nt _TERM, __ , a ")")
          (fun (bar,_,t,_,ket) -> t)]

    | _ when x=nt2c _LAM -> [
        rhs7 
          (a "\\", __, nt _VAR, __, a ".", __, nt _TERM) 
          (fun (lam,_,v,_,dot,_,t) -> Lam(v,t))]

    | _ when x=nt2c _APP -> [
        rhs3
          (nt _TERM, __ , nt _TERM)
          (fun (t1,_,t2) -> App(t1,t2))]

    | _ when x=nt2c _VAR -> [rhs1  (re "[a-z]+")  (fun v -> v)]

    | _ -> failwith "unknown nt"
  in
  ({rules=(Obj.magic rules)}, _S)  (* S is start symbol *)
[@@ocaml.warning "-8"]

(* FIXME this still can't work - have to go via untyped at some point
   :( *)


let lambda_calc_parser = 
  lambda_calc_grammar |> fun (rules_ops, start) -> 
  grammar_to_parser ~eo_ops ~rules_ops ~start

let _ : term P1_core.parser_ = lambda_calc_parser

(* example of use in bin/test_lambda_calc2.ml FIXME *)
*)
