(* lambda calc grammar ---------------------------------------------- *)

(* NOTE uses parsing_dsl2 *)

(* FIXME should probably reduce the number of opens in the following *)
open P1_core
open Parsing_dsl2
open Nt

open Simple_dsl
open Rule_ops

open Grammar_requires
open Elt.Terminals



type term = 
    Lam of string * term | App of term * term | Var of string
      [@@deriving yojson]

let lambda_calc_grammar =
  let _TERM, _LAM, _APP, _VAR, _S = mk_nt(),mk_nt(),mk_nt(),mk_nt(),mk_nt() in
  let __ = re "[ \n]*" in
  let rules = [
    _S -->rhs2  (nt _TERM, eof)  (fun (t,_) -> t);

    (* NOTE this grammar has left recursion (app), and is highly
       ambiguous *)
    (* t = \\ x. t | t1 t2 | x | (t) *)
    _TERM -->rhs1 (nt _LAM)  (fun t -> t);
    _TERM -->rhs1 (nt _APP)  (fun t -> t);
    _TERM -->rhs1 (nt _VAR)  (fun t -> t);

    _TERM -->rhs5 
      (a "(", __, nt _TERM, __ , a ")")
      (fun (bar,_,t,_,ket) -> t);

    _LAM -->rhs7 
      (a "\\", __, nt _VAR, __, a ".", __, nt _TERM) 
      (fun (lam,_,Var v,_,dot,_,t) -> Lam(v,t));

    _APP -->rhs3
      (nt _TERM, __ , nt _TERM)
      (fun (t1,_,t2) -> App(t1,t2));

    _VAR -->rhs1  (re "[a-z]+")  (fun v -> Var v)
  ] 
  in
  (mk_rules_ops rules, _S)  (* S is start symbol *)
[@@ocaml.warning "-8"]


let lambda_calc_parser = 
  lambda_calc_grammar |> fun (rules_ops, start) -> 
  grammar_to_parser ~rules_ops ~start

let _ : term P1_core.parser_ = lambda_calc_parser

(* example of use in bin/test_lambda_calc.ml *)
