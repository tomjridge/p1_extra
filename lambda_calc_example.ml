(* FIXME factor out the latter part of this file with pgf2 *)

open Parsing_dsl

type term = 
    Lam of string * term | App of term * term | Var of string
      [@@deriving yojson]
              

module Make(X:Parsing_dsl.MAKE_REQUIRES) = struct
  
  open X

  let f
      ~ops
      ~eps
      (* terminals *)
      ~a
      ~whitespace_and_comments  (* whitespace and comments *)
      ~re
      ~eof 
      (* nonterminals *)
      ~(_TERM:term nt)
      ~_LAM ~_APP ~_VAR
      ~_S
    =
    begin
      let nt x = ops.ant2aelt x in
      let a x = tm (a x) in
      let __ = tm whitespace_and_comments in
      let ( --> ) x y = ops.add_rule x y in

      _S -->rhs2  (nt _TERM, tm eof)  (fun (t,_) -> t);

      _TERM -->rhs1 (nt _LAM)  (fun t -> t);
      _TERM -->rhs1 (nt _APP)  (fun t -> t);
      _TERM -->rhs1 (nt _VAR)  (fun t -> t);

      _LAM -->rhs7 
        (a "\\", __, nt _VAR, __, a ".", __, nt _TERM) 
        (fun (lam,_,Var v,_,dot,_,t) -> Lam(v,t));

      _APP -->rhs7
        (a "(", __, nt _TERM, __ , nt _TERM, __, a ")")
        (fun (bar,_,t1,_,t2,_,ket) -> App(t1,t2));

      _VAR -->rhs1  (tm (re "[a-z]+"))  (fun v -> Var v);
      _S 
    end


  let _ = f

end (* Make *)

(* FIXME can use phantom typevar to avoid functorization over Nt' *)
module Nt' = struct
  type nt' = S | TERM | LAM | APP | VAR  [@@deriving yojson]
end
include Nt'

include Parsing_dsl.Make(Nt')

(* FIXME this boilerplate *)
include Elt

open Y

module Z = Make(Y)


(* grammar_to_parser, with actions ---------------------------------- *)


(* we want a list of rules, so we coerce the typed rules into an
   untyped version in order to store in the list *)
type rhs'  (* = rhs *)
type rule = { nt: nt'; rhs: rhs' }

let coerce_rule nt rhs = 
  { nt; rhs=(Obj.magic rhs) }

let elt2stringelt : Elt.elt -> string elt = fun x -> x

(* FIXME this repeats a lot of stuff from pgf2 *)
let f'' () =
  let rs = ref [] in
  let add_rule (type a) (nt:a nt) (rhs:a rhs) = 
    (*    R(nt,rhs) |> fun r -> 
          r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;*)
    rs:=(coerce_rule (nt2nt' nt) rhs)::!rs  (* NOTE Obj.magic *)
  in
  (* let add_action rhs a = (rhs,a) in *)
  let star ~sep elt = 
    let sep = aelt2elt sep in
    let elt = aelt2elt elt in
    elt2aelt (E_star(sep,elt)) in
  let plus ~sep elt = 
    let sep = aelt2elt sep in
    let elt = aelt2elt elt in
    elt2aelt (E_plus(sep,elt))
  in
  let ops = { add_rule=add_rule; star; plus; ant2aelt } in
  let a s =  (A s) in
  let whitespace_and_comments =  Ws in
  let re s = (Re s) in
  let eof = Eof in
  let _S = nt'2nt S in
  let _TERM = nt'2nt TERM in
  let _LAM = nt'2nt LAM in
  let _APP = nt'2nt APP in
  let _VAR = nt'2nt VAR in
  let _S = (Z.f 
     ~ops
     ~eps:(a "")
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


   (* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
(* FIXME this can be moved to parsing_dsl *)
let grammar_to_parser' ~(rules:rule list) = 
  let open P1_core in
  let open P1_combinators in
  let open P1_terminals in
  let rec tm_to_parser' = function
    | A s -> a s
    | Upto_a s -> upto_a s
    | Ws -> p1_log ~msg:"ws" ws
    | AZs -> _AZs
    | AZazs -> _AZazs
    | Re s -> re (Str.regexp s)
    | Eof -> eof >> fun _ -> ""
  and tm_to_parser x = tm_to_parser' x >> fun x -> `String x
  and elt_to_parser = function
    | E_star(sep,elt) -> 
      star ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_plus(sep,elt) -> 
      plus ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_sym sym -> sym_to_parser sym
  and sym_to_parser = function
    | S_TM tm -> tm_to_parser tm
    | S_NT nt -> nt_to_parser (nt'2nt nt)
  and nt_to_parser: 'a. 'a nt -> 'a parser_ = fun nt -> fun i ->
      let alt_list xs = alt_list xs >> fun xs -> xs in 
      rules |> List.filter (fun r -> r.nt=nt2nt' nt) |> fun rs ->
      (* Printf.printf "Got %d rules" (List.length rs); *)
      alt_list (
        rs 
        |> List.map (
        fun r -> 
          let rhs = r.rhs in
          let rhs = Obj.magic rhs in
          rhs_to_parser rhs)) i
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

let _ = grammar_to_parser'

(* FIXME do we need eta expansion? *)
let lambda_calc_parser = 
  let _S,rules = (f'' ()) in
  grammar_to_parser'
    ~rules
    _S
