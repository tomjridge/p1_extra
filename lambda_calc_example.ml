(* FIXME factor out the latter part of this file with pgf2 *)

open P1_core

(* make_elt --------------------------------------------------------- *)

module Make_elt(X: sig type 'a nt [@@deriving yojson] end) = struct
  open X
  type _ elt = 
    | E_star: 'b elt * 'a elt -> 'a list elt
    | E_plus: 'b elt * 'a elt -> 'a list elt
    | E_nt: 'a nt -> 'a elt
    | E_tm: 'a parser_ -> 'a elt  
(*  [@@deriving yojson] doesn't work with *)
end


(* make_rhs --------------------------------------------------------- *)

module Make_rhs(X: sig type 'a elt end) = struct
  open X
  type _a  (* a dummy type var *)
  type _b 
  type _c
  type _d
  type _e
  type _f
  type _g

  type 'z rhs = 
      Rhs1 of _a elt * (_a -> 'z)
    | Rhs2 of (_a elt * _b elt) * (_a * _b -> 'z) 
    | Rhs3 of (_a elt * _b elt * _c elt) * (_a*_b*_c -> 'z) 
    | Rhs4 of (_a elt * _b elt * _c elt * _d elt) * (_a*_b*_c*_d -> 'z) 
    | Rhs5 of (_a elt * _b elt * _c elt * _d elt * _e elt) * (_a*_b*_c*_d*_e -> 'z) 
    | Rhs6 of (_a elt * _b elt * _c elt * _d elt * _e elt * _f elt) * 
              (_a*_b*_c*_d*_e*_f -> 'z) 
    | Rhs7 of (_a elt * _b elt * _c elt * _d elt * _e elt * _f elt * _g elt) * 
              (_a*_b*_c*_d*_e*_f*_g -> 'z) 

  let rhs1 : 'a 'b. 'a elt -> ('a -> 'b) -> 'b rhs = fun  x y -> Rhs1(Obj.magic x,Obj.magic y)
  let rhs2 : 'a 'b 'c. 'a elt * 'b elt -> ('a*'b -> 'c) -> 'c rhs = fun x y ->
    Rhs2(Obj.magic x,Obj.magic y)
  let rhs3 : 'a 'b 'c 'd. 'a elt * 'b elt * 'c elt -> ('a*'b*'c -> 'd) -> 'd rhs = fun x y ->
    Rhs3(Obj.magic x,Obj.magic y)
  let rhs4 : 'a 'b 'c 'd 'e. 'a elt * 'b elt * 'c elt * 'd elt -> ('a*'b*'c*'d -> 'e) -> 'e rhs = fun x y ->
    Rhs4(Obj.magic x,Obj.magic y)
  let rhs5 : 'a elt * 'b elt * 'c elt * 'd elt * 'e elt -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs = fun x y -> Rhs5(Obj.magic x,Obj.magic y)
  let rhs6 : 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt -> 
    ('a*'b*'c*'d*'e*'f -> 'g) -> 'g rhs = fun x y -> Rhs6(Obj.magic x,Obj.magic y)
  let rhs7 : 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt * 'g elt -> 
    ('a*'b*'c*'d*'e*'f*'g -> 'h) -> 'h rhs = fun x y -> Rhs7(Obj.magic x,Obj.magic y)
end

(* make_ops --------------------------------------------------------- *)

module Make_ops(
    X:sig 
      type 'a nt
      type 'a elt 
      type 'a rhs
    end) 
= struct
  open X

  type ops = {
    add_rule: 'a. 'a nt -> 'a rhs -> unit;

    (* these cannot be passed as args to a function because we need
       the general type *)
    star: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;
    plus: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;

    ant2aelt: 'a. 'a nt -> 'a elt
  }
end


(* grammar_requires ------------------------------------------------- *)

(* This is more-or-less common to all grammar specs *)
module type GRAMMAR_REQUIRES = sig

  type 'a nt

  type 'a elt  (* things that can appear in a rhs *)

  type 'a rhs

  val rhs1: 'a elt -> ('a -> 'b) -> 'b rhs
  val rhs2: 'a elt * 'b elt -> ('a*'b -> 'c) -> 'c rhs
  val rhs3: 'a elt * 'b elt * 'c elt -> ('a*'b*'c -> 'd) -> 'd rhs
  val rhs4: 'a elt * 'b elt * 'c elt * 'd elt -> ('a*'b*'c*'d -> 'e) -> 'e rhs
  val rhs5: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs
  val rhs6: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt -> 
    ('a*'b*'c*'d*'e*'f -> 'g) -> 'g rhs
  val rhs7: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt * 'g elt -> 
    ('a*'b*'c*'d*'e*'f*'g -> 'h) -> 'h rhs


  (* star and plus are the additional elements that we allow as part of a rhs *)
  type ops = {
    add_rule: 'a. 'a nt -> 'a rhs -> unit;

    (* these cannot be passed as args to a function because we need
       the general type *)
    star: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;
    plus: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;

    ant2aelt: 'a. 'a nt -> 'a elt
  }
end

              


(* lambda calc grammar ---------------------------------------------- *)

type term = 
    Lam of string * term | App of term * term | Var of string
      [@@deriving yojson]


module Make_grammar(X:GRAMMAR_REQUIRES) = struct  
  open X

  let make_grammar
      ~ops
      (* terminals *)
      ~(a:string -> string elt)
      ~(whitespace_and_comments: string elt)  (* whitespace and comments *)
      ~(re:string -> string elt)
      ~(eof:unit elt)
      (* nonterminals *)
      ~(_TERM:term nt)
      ~_LAM ~_APP ~_VAR
      ~_S
    =
    begin
      let nt x = ops.ant2aelt x in
      let __ = whitespace_and_comments in
      let ( --> ) x y = ops.add_rule x y in

      _S -->rhs2  (nt _TERM, eof)  (fun (t,_) -> t);

      _TERM -->rhs1 (nt _LAM)  (fun t -> t);
      _TERM -->rhs1 (nt _APP)  (fun t -> t);
      _TERM -->rhs1 (nt _VAR)  (fun t -> t);

      _LAM -->rhs7 
        (a "\\", __, nt _VAR, __, a ".", __, nt _TERM) 
        (fun (lam,_,Var v,_,dot,_,t) -> Lam(v,t));

      _APP -->rhs7
        (a "(", __, nt _TERM, __ , nt _TERM, __, a ")")
        (fun (bar,_,t1,_,t2,_,ket) -> App(t1,t2));

      _VAR -->rhs1  (re "[a-z]+")  (fun v -> Var v); 
      _S 
    end


  let _ :
ops:X.ops ->a:(string -> string X.elt) ->
whitespace_and_comments:string X.elt ->
re:(string -> string X.elt) ->eof:unit X.elt ->
_TERM:term X.nt ->
_LAM:term X.nt ->
_APP:term X.nt -> _VAR:term X.nt -> _S:term X.nt -> term X.nt
    = make_grammar
end


(* FIXME can use phantom typevar to avoid functorization over Nt' *)
(*
module Untyped_nonterminals = struct
  type nt' = S | TERM | LAM | APP | VAR  [@@deriving yojson]
end
include Untyped_nonterminals
*)

(* FIXME the following could be packaged up into a single functor application *)
module Nonterminals = struct
  (* NOTE that each new instance of a term can be assigned a different type *)
  type 'a nt = S | TERM | LAM | APP | VAR  [@@deriving yojson]
end
include Nonterminals

module Elt = Make_elt(Nonterminals)
include Elt

module Rhs = Make_rhs(Elt)
include Rhs

module Ops_requires = struct
  type 'a nt = 'a Nonterminals.nt
  type 'a elt = 'a Elt.elt
  type 'a rhs = 'a Rhs.rhs
end

module Ops = Make_ops(Ops_requires)
include Ops

module Grammar_requires = struct
  include Nonterminals
  include Elt
  include Rhs
  include Ops
end


(* more generic stuff: untyped rules -------------------------------- *)

type 'a rule = { nt: 'a nt; rhs: 'a rhs }

type untyped_rule =
  | Urule: 'a rule -> untyped_rule


(* generic: grammar_to_parser --------------------------------------- *)

   (* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
(* FIXME this can be moved to parsing_dsl *)
let grammar_to_parser ~(rules:untyped_rule list) = 
  let open P1_core in
  let open P1_combinators in
  let open P1_terminals in

  let rec elt_to_parser' : type a. a elt -> a parser_ = function
    | E_star(sep,elt) -> 
      star ~sep:(elt_to_parser' sep) (elt_to_parser' elt) 
    | E_plus(sep,elt) -> 
      plus ~sep:(elt_to_parser' sep) (elt_to_parser' elt) 
    | E_tm x ->   x
    | E_nt nt -> nt_to_parser nt
  and elt_to_parser : 'a. 'a elt -> 'a parser_ = fun e i -> elt_to_parser' e i  
  (* eta expand this rec call rather than others FIXME except that we
     want E_nt to be evaluated early *)
  and nt_to_parser': 'a nt -> 'a parser_ = fun nt -> 
      let alt_list xs = alt_list xs >> fun xs -> xs in 
      (* get those rules that match nt *)
      let rules = 
        (* FIXME we want to ensure this is executed once only per nt *)
        let rs : 'a rule list ref = ref [] in
        List.iter (function (Urule r) -> 
            if r.nt = Obj.magic nt then rs:=(Obj.magic r)::!rs else ())
          rules;
        !rs
      in
      (* Printf.printf "Got %d rules" (List.length rs); *)
      alt_list (
        rules 
        |> List.map (
          fun r -> 
            let rhs = r.rhs in
            rhs_to_parser rhs)) 
  and nt_to_parser: 'a. 'a nt -> 'a parser_ = fun nt -> (Obj.magic nt_to_parser' nt)
  and rhs_to_parser: 'a. 'a rhs -> 'a parser_ = 
    (* FIXME obviously the following is rather scary *)
    function
    | Rhs1 (s,act) -> (elt_to_parser s >> fun x -> act (Obj.magic x))
    | Rhs2 ((s1,s2),act) -> 
      (elt_to_parser s1) **> (elt_to_parser s2) >> fun x -> act (Obj.magic x)
    | Rhs3 ((s1,s2,s3),act) -> (
        elt_to_parser s1 **>
        elt_to_parser s2 **> 
        elt_to_parser s3)
      >> (fun (x,(y,z)) -> act (Obj.magic (x,y,z)))
    | Rhs4 ((s1,s2,s3,s4),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4)
      >> (fun (x,(y,(z,w))) -> act (Obj.magic (x,y,z,w)))
    | Rhs5 ((s1,s2,s3,s4,s5),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4 **>
        elt_to_parser s5)
      >> (fun (x,(y,(z,(w,u)))) -> act (Obj.magic (x,y,z,w,u)))
    | Rhs6 ((s1,s2,s3,s4,s5,s6),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4 **>
        elt_to_parser s5 **>
        elt_to_parser s6)
      >> (fun (x,(y,(z,(w,(u,v))))) -> act (Obj.magic (x,y,z,w,u,v)))
    | Rhs7 ((s1,s2,s3,s4,s5,s6,s7),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4 **>
        elt_to_parser s5 **>
        elt_to_parser s6 **>
        elt_to_parser s7)
      >> (fun (x,(y,(z,(w,(u,(v,a)))))) -> act (Obj.magic (x,y,z,w,u,v,a)))
  in
  nt_to_parser

let _ : rules:untyped_rule list -> 'a nt -> 'a P1_core.parser_ = grammar_to_parser



(* grammar_to_parser, with actions ---------------------------------- *)

module Grammar = Make_grammar(Grammar_requires)

(* FIXME this repeats a lot of stuff from pgf2 *)
let make_grammar () =
  let rs = ref [] in
  let add_rule (type a) (nt:a nt) (rhs:a rhs) = 
    rs:=(Urule { nt; rhs })::!rs 
  in
  let star ~sep elt = E_star(sep,elt) in
  let plus ~sep elt = E_plus(sep,elt) in
  let ant2aelt x = E_nt x in
  let ops = { add_rule=add_rule; star; plus; ant2aelt } in
  let open P1_terminals in
  let a s = E_tm (a s) in
  let whitespace_and_comments = E_tm (ws) in
  let re s = E_tm (re (Str.regexp s)) in
  let eof = E_tm eof in
  let _S = S in
  let _TERM = TERM in
  let _LAM = LAM in
  let _APP = APP in
  let _VAR = VAR in
  let _S = (Grammar.make_grammar
     ~ops
     (* terminals *)
     ~a
     ~whitespace_and_comments
     ~re
     ~eof
     (* nonterminals *)
      ~_TERM
      ~_LAM ~_APP ~_VAR
      ~_S)
  in
  _S,List.rev !rs


(* FIXME do we need eta expansion? *)
let lambda_calc_parser = 
  let _S,rules = make_grammar () in
  grammar_to_parser
    ~rules
    _S


let _ : term parser_ = lambda_calc_parser
