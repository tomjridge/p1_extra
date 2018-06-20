(* lambda calc grammar ---------------------------------------------- *)

open Parsing_dsl
open Nt
open Grammar_requires
open Elt.Terminals



type term = 
    Lam of string * term | App of term * term | Var of string
      [@@deriving yojson]


let lambda_calc_grammar
    ~_TERM
    ~_LAM ~_APP ~_VAR
    ~_S
  =
  begin
    let nt x = elt_ops.ant2aelt x in
    let __ = whitespace_and_comments in
    let ( --> ) x y = rule_ops.mk_rule x y in
    let rules = [
      _S -->rhs2  (nt _TERM, eof)  (fun (t,_) -> t);

      (* t = \\ x. t | t1 t2 | x *)
      _TERM -->rhs1 (nt _LAM)  (fun t -> t);
      _TERM -->rhs1 (nt _APP)  (fun t -> t);
      _TERM -->rhs1 (nt _VAR)  (fun t -> t);

      _LAM -->rhs7 
        (a "\\", __, nt _VAR, __, a ".", __, nt _TERM) 
        (fun (lam,_,Var v,_,dot,_,t) -> Lam(v,t));

      _APP -->rhs7
        (a "(", __, nt _TERM, __ , nt _TERM, __, a ")")
        (fun (bar,_,t1,_,t2,_,ket) -> App(t1,t2));

      _VAR -->rhs1  (re "[a-z]+")  (fun v -> Var v)
    ] 
    in
    fun f -> f ~rules ~start:_S 
  end


let lambda_calc_parser = 
  lambda_calc_grammar
    ~_TERM:(nt())
    ~_LAM:(nt())
    ~_APP:(nt())
    ~_VAR:(nt())
    ~_S:(nt())
  @@ grammar_to_parser


let _ : term P1_core.parser_ = lambda_calc_parser
