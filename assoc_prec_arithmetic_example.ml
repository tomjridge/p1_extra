(* arithmetic grammar ---------------------------------------------- *)

module type REQUIRED =  sig
  type op = [ `Plus | `Times ]
  type prec = op

  type 'a raw_nt (* without ord and prec *)
  type 'a nt
  val raw_to_nt: 'a raw_nt -> prec -> 'a nt
  val mk_raw: unit -> 'a raw_nt

  type 'a elt
  val int_ : int elt  (* terminal *)

  val raw: 'a raw_nt -> prec -> 'a elt
  val nt: 'a nt -> 'a elt


  type 'a rhs

  val op2elt: op -> unit elt 

  val rhs1: 'a elt -> ('a -> 'b) -> 'b rhs
  val rhs2: 'a elt * 'b elt -> ('a*'b -> 'c) -> 'c rhs
  val rhs3: 'a elt * 'b elt * 'c elt -> ('a*'b*'c -> 'd) -> 'd rhs
  val rhs4: 'a elt * 'b elt * 'c elt * 'd elt -> ('a*'b*'c*'d -> 'e) -> 'e rhs
  val rhs5: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs
  val rhs6: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt -> 
    ('a*'b*'c*'d*'e*'f -> 'g) -> 'g rhs
  val rhs7: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt * 'f elt * 'g elt -> 
    ('a*'b*'c*'d*'e*'f*'g -> 'h) -> 'h rhs

  type untyped_rule

  val mk_rule: 'a nt -> 'a rhs -> untyped_rule

  val get_higher_precedences: prec -> prec list
end


(*

left assoc; * has higher prec. than +

E+ -> E+ + Int | E*
E* -> E* * Int

Alternatively:

Ex -> Ex opx Int | Ey  where y>x

So, given a raw nonterm E and a subscript x we need to produce a list of rhs which includes the Ey for y>x

*)

module Arithmetic_parser(Required:REQUIRED) = struct
  open Required

  let ( --> ) = mk_rule 

  let _E = mk_raw ()

  let no_action x = ()

  (* NOTE the actions are dummies in the following *)
  let rec rules ~raw_ ~prec = 
    assert(raw_ = _E); (* only 1 nt *)
    let _Ex = raw_to_nt _E prec in

    let rule = 
      (* Ex -> Ex opx Int *)
      _Ex -->rhs3 (nt _Ex, op2elt prec, int_)   no_action 
    in
    (* Ex -> Ey where y>x; we just get those higher precedences y and
       add rules Ex -> Ey for each *)
    let rules = 
      prec 
      |> get_higher_precedences 
      |> List.map (fun y ->
          let _Ey = raw _E y in
          (* Ex -> Ey *)
          _Ex -->rhs1 _Ey   no_action)
    in
    rule::rules
end


(*

NOTE an alternative is just to parse using E -> E op Int; then actions
   return the parse tree and the operator precedence; finally discard
   any results where the precedence doesn't respect the grammar
   intention, eg + can have * as left component and int as right

*)
