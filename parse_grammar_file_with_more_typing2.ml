(* use p1 to parse a grammar file *)

(* derived from parse_grammar_file, but using tuples for rhs rather
   than list

   this is a fairly optimal simply-typed DSL for defining grammars with actions
*)

let sq (*single quote*) = {|'|}
let dq = {|"|}

(* example ---------------------------------------------------------- *)

let example = {|

S -> E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}




module Internal_(X:sig 

    type 'a nt

    type 'a elt  (* things that can appear in a rhs *)

    type 'a rhs

    val rhs1: 'a elt -> ('a -> 'b) -> 'b rhs
    val rhs2: 'a elt * 'b elt -> ('a*'b -> 'c) -> 'c rhs
    val rhs3: 'a elt * 'b elt * 'c elt -> ('a*'b*'c -> 'd) -> 'd rhs
    val rhs4: 'a elt * 'b elt * 'c elt * 'd elt -> ('a*'b*'c*'d -> 'e) -> 'e rhs
    val rhs5: 'a elt * 'b elt * 'c elt * 'd elt * 'e elt -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs


    type ops = {
      add_rule: 'a. 'a nt -> 'a rhs -> unit;

      (* these cannot be passed as args to a function because we need
         the general type *)
      star: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;
      plus: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;

      nt2elt: 'a. 'a nt -> 'a elt
    }
    (* val eps: unit sym *)

  end)
= 
struct

  open X


  let f
      ~ops
      ~eps
      (* terminals *)
      ~a
      ~upto_a  
      ~whitespace_and_comments  (* whitespace and comments *)
      ~_AZs ~azAZs ~re
      ~eof 
      (* nonterminals *)
      ~_GRAMMAR
      ~_RULES ~_RULE 
      ~_RHS ~_SYMSACT ~_RHSSEP ~_CODE 
      ~_SYMS ~_VAR_EQ_SYM ~_VAR_EQ 
      ~_SYM ~_NT ~_TM
    =
    begin
      let nt x = ops.nt2elt x in
      let __ = whitespace_and_comments in
      let ( --> ) x y = ops.add_rule x y in

      _GRAMMAR --> rhs4 
        (__,nt _RULES, __, eof )  (fun (_,rs,_,_) -> rs);  

      _RULES --> rhs1 
        (ops.star ~sep:__ (nt _RULE))  (fun rs -> rs); 

      _RULE --> rhs5 
        (nt _NT, __, a "->", __, nt _RHS)  (fun (nt,_,_,_,rhs) -> `Rule(nt,rhs));

      _RHS --> rhs1
        (ops.plus ~sep:(nt _RHSSEP) (nt _SYMSACT))  (fun x -> `Rhs x);

      _RHSSEP --> rhs3
        (__,a "|",__)  (fun _ -> `Ignore);

      _SYMSACT --> rhs3
        (nt _SYMS,__,nt _CODE)  (fun (syms,_,code) -> `Symsact (syms,code));

      _SYMS --> rhs1
        (ops.plus ~sep:__ (nt _VAR_EQ_SYM))  (fun x -> x);

      _CODE --> rhs3 
        (a "{{",upto_a "}}",a "}}")  (fun (_,c,_) -> `Code c);

      _VAR_EQ_SYM --> rhs2 
        (nt _VAR_EQ, nt _SYM) 
        (function | (`Some_var v,x) -> `Var_eq(Some v,x)
                  | (`None_var,x) -> `Var_eq(None,x));
      _VAR_EQ --> rhs2
        (re "[a-z]+",a "=") 
        (fun (v,_) -> `Some_var v );

      _VAR_EQ --> rhs1 eps  (fun _ -> `None_var);

      _SYM --> rhs1 (nt _NT)  (fun x -> `Nt x);
      _SYM --> rhs1 (nt _TM)  (fun x -> `Tm x);

      _TM --> rhs3 
        (a sq,upto_a sq,a sq)  (fun (_,s,_) -> `Sq s);

      _TM --> rhs3
        (a dq,upto_a dq,a dq)  (fun (_,s,_) -> `Dq s);

      _TM --> rhs3
        (a "?",azAZs,a "?")  (fun (_,s,_) -> `Qu s);

      _NT --> rhs1 _AZs  (fun x -> x);

      _GRAMMAR  (* NOTE we return the start nt at the point where we know the max info about its type *)
    end


  let _ = f

(*
ops:X.ops ->®ps:'a X.elt ->ª:(string -> 'b X.elt) ->
upto_a:(string -> 'c X.elt) ->
whitespace_and_comments:'d X.elt ->
_AZs:'e X.elt ->ªzAZs:'f X.elt ->
re:(string -> 'g X.elt) ->®of:'h X.elt ->
_GRAMMAR:([> `Rule of
               'e *
               ([> `Rhs of
                     ([> `Symsact of
                           ([> `Var_eq of
                                 'g option *
                                 ([> `Nt of 'e
                                   | `Tm of
                                       [> `Dq of 'c | `Qu of 'f | `Sq of 'c ]
                                       as 'n ]
                                  as 'm) ]
                            as 'l)
                           list * ([> `Code of 'c ] as 'o) ]
                      as 'k)
                     list ]
                as 'j) ]
          as 'i)
         list X.nt ->
_RULES:'i list X.nt ->
_RULE:'i X.nt ->
_RHS:'j X.nt ->
_SYMSACT:'k X.nt ->
_RHSSEP:[> `Ignore ] X.nt ->
_CODE:'o X.nt ->
_SYMS:'l list X.nt ->
_VAR_EQ_SYM:'l X.nt ->
_VAR_EQ:[ `None_var | `Some_var of 'g ] X.nt ->
_SYM:'m X.nt -> _NT:'e X.nt -> _TM:'n X.nt -> 'i list X.nt*)

end (* Internal_ *)



(* concretization --------------------------------------------------- *)

(* 'a sym is elt? *)

(* NOTE tm' and nt' are specific to the grammar; elt is generic over these *)

(* FIXME just use RE rather than Ws etc *)
type tm' = A of string | Upto_a of string | Re of string | Ws | AZs | AZazs | Eof 
[@@deriving yojson]

type nt' = 
  | Grammar | Rules | Rule 
  | Rhs | Symsact | Code | Rhssep 
  | Syms | Var_eq_sym | Var_eq 
  | Sym | NT | TM  [@@deriving yojson]

(*
let nt_to_string nt' = 
  nt' |> nt'_to_yojson 
  |> function `List (`String s::_) -> s | _ -> (failwith __LOC__)
*)

type sym = S_NT of nt' | S_TM of tm'  [@@deriving yojson]

type elt = 
  | E_star of elt * elt | E_plus of elt * elt 
  | E_sym of sym [@@deriving yojson]

(* type rule = R of (elt * elt list) [@@deriving yojson] *)

(* convenience *)
let nt2elt nt = E_sym (S_NT nt)
let tm2elt tm = E_sym (S_TM tm)


module Y = struct

  type 'a nt = nt'

  type nonrec 'a elt = elt

  type _a  (* a dummy type var *)
  type _b 
  type _c
  type _d
  type _e

  type 'z rhs = 
      Rhs1 of _a elt * (_a -> 'z)
    | Rhs2 of (_a elt * _b elt) * (_a * _b -> 'z) 
    | Rhs3 of (_a elt * _b elt * _c elt) * (_a*_b*_c -> 'z) 
    | Rhs4 of (_a elt * _b elt * _c elt * _d elt) * (_a*_b*_c*_d -> 'z) 
    | Rhs5 of (_a elt * _b elt * _c elt * _d elt * _e elt) * (_a*_b*_c*_d*_e -> 'z) 

  let rhs1 : 'a 'b. 'a elt -> ('a -> 'b) -> 'b rhs = fun  x y -> Rhs1(Obj.magic x,Obj.magic y)
  let rhs2 : 'a 'b 'c. 'a elt * 'b elt -> ('a*'b -> 'c) -> 'c rhs = fun x y ->
    Rhs2(Obj.magic x,Obj.magic y)
  let rhs3 : 'a 'b 'c 'd. 'a elt * 'b elt * 'c elt -> ('a*'b*'c -> 'd) -> 'd rhs = fun x y ->
    Rhs3(Obj.magic x,Obj.magic y)
  let rhs4 : 'a 'b 'c 'd 'e. 'a elt * 'b elt * 'c elt * 'd elt -> ('a*'b*'c*'d -> 'e) -> 'e rhs = fun x y ->
    Rhs4(Obj.magic x,Obj.magic y)
  let rhs5 : 'a elt * 'b elt * 'c elt * 'd elt * 'e elt -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs = fun x y -> Rhs5(Obj.magic x,Obj.magic y)

    type ops = {
      add_rule: 'a. 'a nt -> 'a rhs -> unit;

      (* these cannot be passed as args to a function because we need
         the general type *)
      star: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;
      plus: 'a 'b. sep:'a elt -> 'b elt -> 'b list elt;

      nt2elt: 'a. 'a nt -> 'a elt
    }
end

open Y

module Z = Internal_(Y)

let _ = Z.f

(* grammar_to_parser, with actions ---------------------------------- *)


(* we want a list of rules, so we coerce the typed rules into an
   untyped version in order to store in the list *)
type rhs'  (* = rhs *)
type rule = { nt: nt'; rhs: rhs' }

let coerce_rule nt rhs = 
  { nt; rhs=(Obj.magic rhs) }

let f'' () =
  let rs = ref [] in
  let add_rule (type a) (nt:a nt) (rhs:a rhs) = 
    (*    R(nt,rhs) |> fun r -> 
          r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;*)
    rs:=(coerce_rule nt rhs)::!rs  (* NOTE Obj.magic *)
  in
  (* let add_action rhs a = (rhs,a) in *)
  let star ~sep nt = E_star(sep,nt) in
  let plus ~sep nt = E_plus(sep,nt) in
  let ops = { add_rule=add_rule; star; plus; nt2elt } in
  let header () = () in
  let a s = tm2elt (A s) in
  let upto_a s = tm2elt (Upto_a s) in
  let whitespace_and_comments = tm2elt Ws in
  let _AZs = tm2elt AZs in
  let azAZs = tm2elt AZazs in
  let re s = tm2elt (Re s) in
  let eof = tm2elt Eof in
  let _GRAMMAR = Grammar in
  let _RULES = Rules in
  let _RULE = Rule in
  let _RHS = Rhs in
  let _SYMSACT = Symsact in
  let _CODE = Code in
  let _RHSSEP = Rhssep in
  let _SYMS = Syms in
  let _VAR_EQ_SYM = Var_eq_sym in
  let _VAR_EQ = Var_eq in
  let _SYM = Sym in
  let _NT = NT in
  let _TM = TM in  (* TM is a nonterminal - it expands to 'x' etc *)
  let _ = _GRAMMAR in
  let _GRAMMAR = (Z.f 
     ~ops
     ~eps:(a "")
     (* terminals *)
     ~a
     ~upto_a  
     ~whitespace_and_comments  (* whitespace and comments *)
     ~_AZs ~azAZs ~re
     ~eof 
     (* nonterminals *)
     ~_GRAMMAR ~_RULES ~_RULE 
     ~_RHS ~_SYMSACT ~_CODE ~_RHSSEP
     ~_SYMS ~_VAR_EQ_SYM ~_VAR_EQ  
     ~_SYM ~_NT ~_TM)
  in
  _GRAMMAR,List.rev !rs

let _ = f''

(* we can call f'' and get hold of the rules *)
let rs = f'' ()

(* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
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
  (*and elt_to_parser: 'a. 'a sym -> 'a parser_ = fun x -> failwith ""*)
  and elt_to_parser = function
    | E_star(sep,elt) -> 
      star ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_plus(sep,elt) -> 
      plus ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_sym sym -> sym_to_parser sym
  and sym_to_parser = function
    | S_TM tm -> tm_to_parser tm
    | S_NT nt -> nt_to_parser nt
  and nt_to_parser: 'a. 'a nt -> 'a parser_ = fun nt ->
      let alt_list xs = alt_list xs >> fun xs -> xs in 
      rules |> List.filter (fun r -> r.nt=nt) |> fun rs ->
      Printf.printf "Got %d rules" (List.length rs);
      alt_list (
        rs 
        |> List.map (
        fun r -> 
          let rhs = r.rhs in
          let rhs = Obj.magic rhs in
          rhs_to_parser rhs))
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
  in
  nt_to_parser

let _ = grammar_to_parser'


let _GRAMMAR,rules = (f'' ())

let rules_without_actions = 
  rules |> List.map (fun r ->
      (r.nt,r.rhs |> Obj.magic |> function
         | Rhs1 (x,_) -> [x]
         | Rhs2 ((x,y),_) -> [x;y]
         | Rhs3 ((x,y,z),_) -> [x;y;z]
         | Rhs4 ((x,y,z,w),_) -> [x;y;z;w]
         | Rhs5 ((x,y,z,w,u),_) -> [x;y;z;w;u]))


let grammar_to_parser = 
  grammar_to_parser'
    ~rules
    (* (!Z.grammar_ref |> fun (Some s) -> s) *)
    (* (e_nt Grammar) *)
    _GRAMMAR


(* we now have a parser that can parse the example text (itself a
   grammar file) such as above *)

let _ = grammar_to_parser

(* type of grammar_to_parser is given as 'a parser, but this is not
   correct given the type of _GRAMMAR *)
