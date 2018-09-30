(* logic grammar, usign HOL ascii syntax ---------------------------- *)


(* FIXME should probably reduce the number of opens in the following *)
open P1_core
open Parsing_dsl2
open Nt

open Simple_dsl
open Rule_ops

open Grammar_requires
open Elt.Terminals

type var = string [@@deriving yojson]

type form = 
  | Forall of var * form 
  | Exists of var * form
  | And of form * form
  | Or of form * form
  | Imp of form * form
  | Atom of string
[@@deriving yojson]

let logic_grammar =
  let _FORM,_S = mk_nt(),mk_nt() in
  let __ = re "[ \n]*" in
  let var = re "[a-z]+" in
  let rules = [
    _S -->rhs2  (nt _FORM, eof)  (fun (t,_) -> t);

    (* forall *)
    _FORM-->rhs7 
      (a "!",__,var,__,a ".",__,nt _FORM)
      (fun (_,_,v,_,_,_,f) -> Forall (v,f));

    (* exists *)
    _FORM-->rhs7 
      (a "?",__,var,__,a ".",__,nt _FORM)
      (fun (_,_,v,_,_,_,f) -> Exists (v,f));

    (* and *)
    _FORM-->rhs5
      (nt _FORM,__,a {|/\|}, __,nt _FORM)
      (fun (f1,_,_,_,f2) -> And (f1,f2));

    (* or *)
    _FORM-->rhs5
      (nt _FORM,__,a {|\/|}, __,nt _FORM)
      (fun (f1,_,_,_,f2) -> Or (f1,f2));

    (* imp *)
    _FORM-->rhs5
      (nt _FORM,__,a {|-->|}, __,nt _FORM)
      (fun (f1,_,_,_,f2) -> Imp (f1,f2));

    (* atom *)
    _FORM -->rhs3
      (a "`", upto_a "`", a"`")
      (fun (_,s,_) -> Atom s);

    _FORM -->rhs5
      (a "(", __,nt _FORM, __, a ")")
      (fun (_,_,f,_,_) -> f)
    
  ] 
  in
  (mk_rules_ops rules, _S)  (* S is start symbol *)
[@@ocaml.warning "-8"]


let logic_parser = 
  logic_grammar |> fun (rules_ops, start) -> 
  grammar_to_parser ~rules_ops ~start

let _ : form P1_core.parser_ = logic_parser

(* example of use in bin/test_lambda_calc.ml *)

