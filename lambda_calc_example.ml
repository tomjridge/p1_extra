(* lambda calc grammar ---------------------------------------------- *)

open Parsing_dsl
open Nt
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
  (`Rules rules, `Start _S)


let lambda_calc_parser = 
  lambda_calc_grammar |> fun (`Rules rules, `Start start) -> 
  grammar_to_parser ~rules ~start

let _ : term P1_core.parser_ = lambda_calc_parser

(* example of use in bin/test_lambda_calc.ml *)
