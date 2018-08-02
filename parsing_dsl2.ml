(* FIXME factor out the latter part of this file with pgf2 *)

(* based on parsing dsl but with arbitrary elts *)

(* FIXME at the moment this is not used because we need to decide how
   to handle additional elts such as star when working with an Earley
   backend; perhaps we just view star and plus as grammar
   transformers?

   star ~sep ~elt = let nt = mk_nt () in ... this really is not an elt
   since it creates another nt and adds rules for that, but we would
   really like to treat it as such

 *)

open P1_core


module Nt = (
struct
  type 'a nt = int (* could be anything that is hashable; probably also needs some concrete repn. for p1 context *)
  let counter = ref 0
  let mk_nt () = counter:=!counter+1; !counter
  let nt2int nt = nt
end : sig 
  type 'a nt
  val mk_nt: unit -> 'a nt 
  val nt2int: 'a nt -> int
end)
open Nt

(* make_elt --------------------------------------------------------- *)

type nt2p = {
  nt2p: 'a. 'a nt -> 'a parser_
}

(* We want to keep the nature of the elts flexible so we can
   incorporate operators like star etc *)
module Make(X:sig 
    type 'a e_other      
    type eo_ops = {
      eo2p: 'a. 'a e_other -> 'a parser_
    }
  end) = struct

  open X


  module Nt' = struct
    type nonrec 'a nt = 'a nt
  end

  open Nt'


  module Elt = struct
    type _ elt = 
      | E_nt: 'a nt -> 'a elt
      | E_tm: 'a parser_ -> 'a elt  
      | E_other: 'a e_other -> 'a elt  (* star, plus etc *)
    (*  [@@deriving yojson] doesn't work with *)


    module Terminals = struct 
      open P1_terminals
      (* generic; could even add to Elt.Terminals ; but still need to explicitly open *)
      let a s = E_tm (a s)
      let upto_a s = E_tm (upto_a s)
      let whitespace_and_comments = E_tm ws
      let re s = E_tm (re (Str.regexp s))
      let eof = E_tm eof
    end

    let ant2aelt x = E_nt x 

    type elt_ops = {
      ant2aelt: 'a. 'a nt -> 'a elt
    }

    (* FIXME just bind using let in this module? or is the problem that we want to avoid this in GRAMMAR_REQUIRES? *)
    let elt_ops = { ant2aelt }

  end


  (* make_rhs --------------------------------------------------------- *)

  module Rhs = struct
    open Elt
    type _a  (* a dummy type var *)
    type _b 
    type _c
    type _d
    type _e
    type _f
    type _g

    (* FIXME can't we have general types now and use a GADT? *)
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

  (* make_rule_ops ----------------------------------------------------- *)

  module Rule_ops
  = struct
    open Elt
    open Rhs

    type rules_ops = {
      rules: 'a. 'a nt -> 'a rhs list;
    }

  end


  (* grammar_requires ------------------------------------------------- *)

  (* This is more-or-less common to all grammar specs *)
  module type GRAMMAR_REQUIRES = sig

    type 'a nt

    (* FIXME factor our the elt part, include vals rather than ops *)
    type 'a elt  (* things that can appear in a rhs *)

    type elt_ops = {
      ant2aelt: 'a. 'a nt -> 'a elt
    }

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

    (* one way of expressing the grammar *)
    type rules_ops = {
      rules: 'a. 'a nt -> 'a rhs list;
    }

  end




  module Ops_requires = struct
    type nonrec 'a nt = 'a nt
    type 'a elt = 'a Elt.elt
    type 'a rhs = 'a Rhs.rhs
  end



  module Grammar_requires = struct
    include Nt'
    include Elt
    include Rhs
    include Rule_ops
  end
  open Grammar_requires



  (* generic: grammar_to_parser --------------------------------------- *)

  (* NOTE the following is more-or-less independent of the nature of the
     terminals or the nonterminals *)
  (* FIXME work with a function from 'a nt -> 'a rhs *)
  let grammar_to_parser ~eo_ops ~(rules_ops:rules_ops)  = 
    let open P1_core in
    let open P1_combinators in
    let open P1_terminals in

    let rec elt_to_parser' : type a. a elt -> a parser_ = function
      | E_tm x ->   x
      | E_nt nt -> nt_to_parser nt
      | E_other e -> eo_ops.eo2p e
    and elt_to_parser : 'a. 'a elt -> 'a parser_ = fun e i -> elt_to_parser' e i  
    (* eta expand this rec call rather than others FIXME except that we
       want E_nt to be evaluated early *)
    and nt_to_parser': 'a nt -> 'a parser_ = fun nt -> 
      let alt_list xs = alt_list xs >> fun xs -> xs in 
      (* get those rules that match nt *)
      let rules = rules_ops.rules nt in
      alt_list (
        rules 
        |> List.map (
          fun rhs -> 
            rhs_to_parser rhs))
    and nt_to_parser: 'a. 'a nt -> 'a parser_ = 
      fun nt -> 
        let nt_s = nt |> nt2int |> string_of_int in
        P1_core.check nt_s (Obj.magic nt_to_parser' nt)
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
    fun ~start -> nt_to_parser start

  let _ : eo_ops:eo_ops -> rules_ops:rules_ops -> start:'a nt -> 'a P1_core.parser_ = grammar_to_parser



  type untyped_rhs
  let arhs2urhs (rhs:'a rhs) : untyped_rhs = Obj.magic rhs
  let urhs2arhs (nt:'a nt) (rhs:untyped_rhs) : 'a rhs = Obj.magic rhs

  type untyped_rule = Urule: 'a nt * 'a rhs -> untyped_rule

  let ( --> ) x y = Urule(x,y)  

  let nt x = elt_ops.ant2aelt x


  

  (* NOTE we assume equality and compare "work" for 'a nt *)
  (* FIXME perhaps we should develop a monadic dsl for the grammar, so
     that we don't have to work with untyped rules *)
  (* or perhaps the grammar is expressed as a function from NT to rhs list? *)
  let mk_rules_ops us = 
    let tbl = Hashtbl.create 10 in
    begin
      (* init tbl *)
      (* first we make a map of nt -> urhs *)
      us |> List.map (function Urule(nt,rhs) -> (nt2int nt,arhs2urhs rhs)) |> fun rules ->
      List.iter (fun (nt,urhs) -> 
          let existing = 
            try 
              Hashtbl.find tbl nt 
            with Not_found -> []
          in
          Hashtbl.replace tbl nt (urhs::existing)) rules
    end;
    let rules = 
      fun nt ->
        try 
          Hashtbl.find tbl (nt2int nt) |> List.map (urhs2arhs nt)
        with Not_found -> []
    in
    { rules }
end


(* no other elts ---------------------------------------------------- *)

module Simple_elts = struct
  type 'a e_other  (* no other elements *)
  type eo_ops = {
    eo2p: 'a. 'a e_other -> 'a parser_
  }
end

module Simple_dsl = struct
  include Make(Simple_elts)

  let eo_ops = Simple_elts.{eo2p=fun eo -> failwith "impossible: no other elts"}
                 
  let grammar_to_parser ~rules_ops ~start = grammar_to_parser ~eo_ops ~rules_ops ~start
end
