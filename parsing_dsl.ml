(* a simple parsing dsl *)

(* This is more-or-less common to all grammar specs *)
module type MAKE_REQUIRES = sig

  type 'a nt

  type 'a elt  (* things that can appear in a rhs *)

  type 'a rhs

  type tm

  val tm : tm -> string elt

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

(* FIXME just use RE rather than Ws etc *)
module Tm' = struct
  type tm' = A of string | Upto_a of string | Re of string | Ws | AZs | AZazs | Eof 
  [@@deriving yojson]
end
include Tm'

module Make(X:sig 
    type nt' 
    val nt'_of_yojson: Yojson.Safe.json -> (nt',string)result
    val nt'_to_yojson: nt' -> Yojson.Safe.json
  end) = struct

  open X

  type sym = S_NT of nt' | S_TM of tm'  [@@deriving yojson]

  module Elt = struct
    type elt = 
      | E_star of elt * elt | E_plus of elt * elt 
      | E_sym of sym [@@deriving yojson]
  end
  open Elt

  (* type rule = R of (elt * elt list) [@@deriving yojson] *)

  (* convenience *)
  let nt2elt nt = E_sym (S_NT nt)
  let tm2elt tm = E_sym (S_TM tm)


  module type MAKE_REQUIRES' = sig
    include MAKE_REQUIRES
    val nt2nt' : 'a nt -> nt'
    val nt'2nt : nt' -> 'a nt
    val aelt2elt: 'a elt -> Elt.elt
    val elt2aelt: Elt.elt -> 'a elt
    val ant2aelt : 'a nt -> 'a elt
  end

  module Y = struct

    (* NOTE we can't have 'a nt = nt' because the typechecker then
       identifies 'a nt and 'b nt, which is not what we want *)
    type 'a nt  (* = nt' *)

    let nt2nt' : 'a nt -> nt' = fun x -> Obj.magic x
    let nt'2nt : nt' -> 'a nt = fun x -> Obj.magic x

    (* surprisingly this is OK and doesn't mess up the
       typechecking... perhaps because we are only interested in 'a nt? *)
    type 'a elt = Elt.elt

    let aelt2elt = fun e -> e
    let elt2aelt = fun e -> e

    let ant2aelt : 'a nt -> 'a elt  = fun nt -> E_sym (S_NT (nt2nt' nt))


    type tm = tm'
    let tm : tm -> string elt = fun tm -> E_sym(S_TM tm)

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

    type ops = {
      add_rule: 'a. 'a nt -> 'a rhs -> unit;

      (* these cannot be passed as args to a function because we need
         the general type *)
      star: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;
      plus: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;

      ant2aelt: 'a. 'a nt -> 'a elt
    }
  end
end
