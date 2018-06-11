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

    type 'a sym

    type 'a rhs

    val rhs1: 'a sym -> ('a -> 'b) -> 'b rhs
    val rhs2: 'a sym * 'b sym -> ('a*'b -> 'c) -> 'c rhs
    val rhs3: 'a sym * 'b sym * 'c sym -> ('a*'b*'c -> 'd) -> 'd rhs
    val rhs4: 'a sym * 'b sym * 'c sym * 'd sym -> ('a*'b*'c*'d -> 'e) -> 'e rhs
    val rhs5: 'a sym * 'b sym * 'c sym * 'd sym * 'e sym -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs


    type ops = {
      add_rule: 'a. 'a sym -> 'a rhs -> unit;

      (* these cannot be passed as args to a function because we need
         the general type *)
      star: 'a 'b. sep:'a sym -> 'b sym -> 'b list sym;
      plus: 'a 'b. sep:'a sym -> 'b sym -> 'b list sym
    }
    (* val eps: unit sym *)

  end)
= 
struct

  open X

  let f
    ~ops
      ~eps
      ~header 
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
      let __ = whitespace_and_comments in
      let ( --> ) x y = ops.add_rule x y in

      _GRAMMAR --> rhs4 
        (__,_RULES, __, eof )  (fun (_,rs,_,_) -> rs);  

      _RULES --> rhs1 
        (ops.star ~sep:__ _RULE)  (fun rs -> rs); 

      _RULE --> rhs5 
        (_NT, __, a "->", __, _RHS)  (fun (nt,_,_,_,rhs) -> `Rule(nt,rhs));

      _RHS --> rhs1
        (ops.plus ~sep:_RHSSEP _SYMSACT)  (fun x -> `Rhs x);

      _RHSSEP --> rhs3
        (__,a "|",__)  (fun _ -> `Ignore);

      _SYMSACT --> rhs3
        (_SYMS,__,_CODE)  (fun (syms,_,code) -> `Symsact (syms,code));

      _SYMS --> rhs1
        (ops.plus ~sep:__ _VAR_EQ_SYM)  (fun x -> x);

      _CODE --> rhs3 
        (a "{{",upto_a "}}",a "}}")  (fun (_,c,_) -> `Code c);

      _VAR_EQ_SYM --> rhs2 
        (_VAR_EQ, _SYM) 
        (function | (`Some_var v,x) -> `Var_eq(Some v,x)
                  | (`None_var,x) -> `Var_eq(None,x));
      _VAR_EQ --> rhs2
        (re "(a-z)+",a "=") 
        (fun (v,_) -> `Some_var v );

      _VAR_EQ --> rhs1 eps  (fun _ -> `None_var);

      _SYM --> rhs1 _NT  (fun x -> `Nt x);
      _SYM --> rhs1 _TM  (fun x -> `Tm x);

      _TM --> rhs3 
        (a sq,upto_a sq,a sq)  (fun (_,s,_) -> `Sq s);

      _TM --> rhs3
        (a dq,upto_a dq,a dq)  (fun (_,s,_) -> `Dq s);

      _TM --> rhs3
        (a "?",azAZs,a "?")  (fun (_,s,_) -> `Qu s);

      _NT --> rhs1 _AZs  (fun x -> x);

      _GRAMMAR
    end


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

let nt_to_string nt' = 
  nt' |> nt'_to_yojson 
  |> function `List (`String s::_) -> s | _ -> (failwith __LOC__)

type elt = 
  | E_star of elt * elt | E_plus of elt * elt 
  | E_NT of nt' | E_TM of tm' [@@deriving yojson]

(* type rule = R of (elt * elt list) [@@deriving yojson] *)


module Y = struct
  type 'a sym = elt

  type _a  (* a dummy type var *)
  type _b 
  type _c
  type _d
  type _e

  type 'z rhs = 
      Rhs1 of _a sym * (_a -> 'z)
    | Rhs2 of (_a sym * _b sym) * (_a * _b -> 'z) 
    | Rhs3 of (_a sym * _b sym * _c sym) * (_a*_b*_c -> 'z) 
    | Rhs4 of (_a sym * _b sym * _c sym * _d sym) * (_a*_b*_c*_d -> 'z) 
    | Rhs5 of (_a sym * _b sym * _c sym * _d sym * _e sym) * (_a*_b*_c*_d*_e -> 'z) 

(*
    | Rhs1: 'a sym * ('a -> 'z) -> 'z rhs
    | Rhs2: ('a sym * 'b sym) * ('a * 'b -> 'z) -> 'z rhs
    | Rhs3: ('a sym * 'b sym * 'c sym) * ('a*'b*'c -> 'z) -> 'z rhs
    | Rhs4: ('a sym * 'b sym * 'c sym * 'd sym) * ('a*'b*'c*'d -> 'z) -> 'z rhs
    | Rhs5: ('a sym * 'b sym * 'c sym * 'd sym * 'e sym) * ('a*'b*'c*'d*'e -> 'z) -> 'z rhs
*)

  let rhs1 : 'a 'b. 'a sym -> ('a -> 'b) -> 'b rhs = fun  x y -> Rhs1(Obj.magic x,Obj.magic y)
  let rhs2 : 'a 'b 'c. 'a sym * 'b sym -> ('a*'b -> 'c) -> 'c rhs = fun x y ->
    Rhs2(Obj.magic x,Obj.magic y)
  let rhs3 : 'a 'b 'c 'd. 'a sym * 'b sym * 'c sym -> ('a*'b*'c -> 'd) -> 'd rhs = fun x y ->
    Rhs3(Obj.magic x,Obj.magic y)
  let rhs4 : 'a 'b 'c 'd 'e. 'a sym * 'b sym * 'c sym * 'd sym -> ('a*'b*'c*'d -> 'e) -> 'e rhs = fun x y ->
    Rhs4(Obj.magic x,Obj.magic y)
  let rhs5 : 'a sym * 'b sym * 'c sym * 'd sym * 'e sym -> ('a*'b*'c*'d*'e -> 'f) -> 'f rhs = fun x y -> Rhs5(Obj.magic x,Obj.magic y)

  type ops = {
    add_rule: 'a. 'a sym -> 'a rhs -> unit;

    (* these cannot be passed as args to a function because we need
       the general type *)
    star: 'a 'b. sep:'a sym -> 'b sym -> 'b list sym;
    plus: 'a 'b. sep:'a sym -> 'b sym -> 'b list sym
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

let coerce_rule sym rhs = 
  let E_NT nt = sym in  (* we also assume all the lhs-s are nts *)
  { nt; rhs=(Obj.magic rhs) }

let f'' () =
  let rs = ref [] in
  let add_rule (type a) (nt:a sym) (rhs:a rhs) = 
    (*    R(nt,rhs) |> fun r -> 
          r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;*)
    rs:=(coerce_rule nt rhs)::!rs  (* NOTE Obj.magic *)
  in
  (* let add_action rhs a = (rhs,a) in *)
  let star ~sep nt = E_star(sep,nt) in
  let plus ~sep nt = E_plus(sep,nt) in
  let ops = { add_rule=add_rule; star; plus } in
  let header () = () in
  let a s = E_TM(A s) in
  let upto_a s = E_TM(Upto_a s) in
  let whitespace_and_comments = E_TM Ws in
  let _AZs = E_TM AZs in
  let azAZs = E_TM AZazs in
  let re s = E_TM (Re s) in
  let eof = E_TM Eof in
  let _GRAMMAR = E_NT Grammar in
  let _RULES = E_NT Rules in
  let _RULE = E_NT Rule in
  let _RHS = E_NT Rhs in
  let _SYMSACT = E_NT Symsact in
  let _CODE = E_NT Code in
  let _RHSSEP = E_NT Rhssep in
  let _SYMS = E_NT Syms in
  let _VAR_EQ_SYM = E_NT Var_eq_sym in
  let _VAR_EQ = E_NT Var_eq in
  let _SYM = E_NT Sym in
  let _NT = E_NT NT in
  let _TM = E_NT TM in  (* TM is a nonterminal - it expands to 'x' etc *)
  ignore (Z.f 
            ~ops
            ~eps:(a "")
            ~header 
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
            ~_SYM ~_NT ~_TM);
  List.rev !rs

let _ = f''

(* we can call f'' and get hold of the rules *)
let rs = f'' ()

(*
(* we have to name the type of the result *)
type res = [`List of res list | `String of string ] 

(* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
let grammar_to_parser' ~(rules:rule list) = 
  let open P1_core in
  let open P1_combinators in
  let open P1_terminals in
  let rec rhs_to_parser: res rhs -> res parser_ = 
    (* FIXME obviously the following is rather scary *)
    function
    | Rhs1 (s,act) -> (elt_to_parser s >> act)
    | Rhs2 ((s1,s2),act) -> 
      (elt_to_parser s1) **> (elt_to_parser s2) >> act
    | Rhs3 ((s1,s2,s3),act) -> (
        elt_to_parser s1 **>
        elt_to_parser s2 **> 
        elt_to_parser s3)
      >> (fun (x,(y,z)) -> act (x,y,z))
    | Rhs4 ((s1,s2,s3,s4),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4)
      >> (fun (x,(y,(z,w))) -> act (x,y,z,w))
    | Rhs5 ((s1,s2,s3,s4,s5),act) -> (
        elt_to_parser s1 **> 
        elt_to_parser s2 **> 
        elt_to_parser s3 **>
        elt_to_parser s4 **>
        elt_to_parser s5)
      >> (fun (x,(y,(z,(w,u)))) -> act (x,y,z,w,u))
  and tm_to_parser' = function
    | A s -> a s
    | Upto_a s -> upto_a s
    | Ws -> ws
    | AZs -> _AZs
    | AZazs -> _AZazs
    | Re s -> re (Str.regexp s)
    | Eof -> eof >> fun _ -> ""
  and tm_to_parser x = tm_to_parser' x >> fun x -> `String x
  (*and elt_to_parser: 'a. 'a sym -> 'a parser_ = fun x -> failwith ""*)

  and elt_to_parser : res sym -> res parser_ = function
    | E_star(sep,elt) -> 
      star ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_plus(sep,elt) -> 
      plus ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `List xs
    | E_TM tm -> tm_to_parser tm
    | E_NT nt -> 
      let alt_list xs = alt_list xs >> fun xs -> xs in 
      rules |> List.filter (fun r -> r.nt=nt) |> fun rs ->
      alt_list (
        rs 
        |> List.map (
        fun r -> 
          let rhs = r.rhs in
          let rhs : res rhs = Obj.magic rhs in
          rhs_to_parser rhs))
      
  in
  elt_to_parser

let grammar_to_parser = 
  grammar_to_parser'
    ~rules:(f'' ())
    (E_NT Grammar)


let _ = grammar_to_parser



*)
