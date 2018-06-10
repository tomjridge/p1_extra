(* use p1 to parse a grammar file *)

(* derived from parse_grammar_file, but using tuples for rhs rather
   than list

   this is a fairly optimal simply-typed DSL for defining grammars with actions
 *)

let sq (*single quote*) = {|'|}
let dq = {|"|}


module Internal_(X:sig 

    type 'a sym

    val mk_sym: unit -> 'a sym

    type 'a rule

    type ('a,'b) action

    type grammar

    val empty_grammar : grammar

    type 'a rhs

    val rhs1: 'a sym -> 'a rhs
    val rhs2: 'a sym * 'b sym -> ('a*'b) rhs
    val rhs3: 'a sym * 'b sym * 'c sym -> ('a*'b*'c) rhs
    val rhs4: 'a sym * 'b sym * 'c sym * 'd sym -> ('a*'b*'c*'d) rhs
    val rhs5: 'a sym * 'b sym * 'c sym * 'd sym * 'e sym -> ('a*'b*'c*'d*'e) rhs

    val add_rule: grammar ref -> 'a sym -> 'a rhs -> unit

    val add_action: 'a rhs -> ('a -> 'b) -> 'b rhs

end)
= 
struct

  open X

  (* example ---------------------------------------------------------- *)

  let example = {|

S -> E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}


  let g = ref empty_grammar

  (* NOTE eta expansion *)
  let add_rule x y = add_rule g x y


  let f
      ~star ~plus ~opt ~eps
      ~header 
      (* terminals *)
      ~a
      ~upto_a  
      ~whitespace_and_comments  (* whitespace and comments *)
      ~_AZs ~azAZs ~re
      ~eof 
      (* nonterminals *)
      ~_GRAMMAR ~_RULES ~_RULE 
      ~_RHS ~_SYMSACT ~_RHSSEP ~_CODE 
      ~_SYMS ~_VAR_EQ_SYM ~_VAR_EQ 
      ~_SYM ~_NT ~_TM
    =
    begin
      let __ = whitespace_and_comments in
      let ( --> ) x y = add_rule x y in
      let ( ** ) = add_action in

      (* better syntax? *)
      let rhs2 syms act = rhs2 syms ** act in


      _GRAMMAR --> rhs4 
        (__,_RULES, __, eof )  ** (fun (_,rs,_,_) -> rs);  
      _RULES --> rhs1 
        (star ~sep:__ _RULE) ** (fun rs -> rs); 

      _RULE --> rhs5 
        (_SYM, __, a "->", __, _RHS) ** (fun (nt,_,_,_,rhs) -> `Rule(nt,rhs));
      _RHS --> rhs1
        (plus ~sep:_RHSSEP _SYMSACT) ** (fun x -> `Rhs x);
      
      _RHSSEP --> rhs3
        (__,a "|",__) ** (fun _ -> `Ignore);

      _SYMSACT --> rhs3
        (_SYMS,__,_CODE) ** (fun (syms,_,code) -> `Symsact (syms,code));
      _SYMS --> rhs1
        (plus ~sep:__ _VAR_EQ_SYM) ** (fun x -> x);

      _CODE --> rhs3 
        (a "{{",upto_a "}}",a "}}") ** (fun (_,c,_) -> `Code c);

      _VAR_EQ_SYM --> rhs2 
        (_VAR_EQ, _SYM) 
        (function | (`Some_var v,x) -> `Var_eq(Some v,x)
                  | (`None_var,x) -> `Var_eq(None,x));
      _VAR_EQ --> rhs2
        (re "(a-z)+",a "=") 
        (fun (v,_) -> `Some_var v );
      _VAR_EQ --> rhs1 eps ** (fun _ -> `None_var);

      _SYM --> rhs1 _NT ** (fun x -> x);
      _SYM --> rhs1 _TM ** (fun x -> x);

      _TM --> rhs3 
        (a sq,upto_a sq,a sq) ** (fun (_,s,_) -> `Sq s);
      _TM --> rhs3
        (a dq,upto_a dq,a dq) ** (fun (_,s,_) -> `Dq s);
      _TM --> rhs3
        (a "?",azAZs,a "?") ** (fun (_,s,_) -> `Qu s);
      _NT --> rhs1 _AZs ** (fun x -> `Nt x);
    end

  (* FIXME can we improve the syntax of the above? 

     eg rhs2 (X,Y) (fun (x,y) -> ... ) ie miss out the ** operator as
     rhs2 above


     FIXME what if we want to define a grammar without actions... can
     this be done nicely? *)


  (* FIXME convert rest of parse_grammar_file *)

end

(*
module Internal_ :
  functor
    (X : sig
           type 'a sym
           val mk_sym : unit -> 'a sym
           type 'a rule
           type ('a, 'b) action
           type grammar
           val empty_grammar : grammar
           type 'a rhs
           val rhs1 : 'a sym -> 'a rhs
           val rhs2 : 'a sym * 'b sym -> ('a * 'b) rhs
           val rhs3 : 'a sym * 'b sym * 'c sym -> ('a * 'b * 'c) rhs
           val rhs4 :
             'a sym * 'b sym * 'c sym * 'd sym -> ('a * 'b * 'c * 'd) rhs
           val rhs5 :
             'a sym * 'b sym * 'c sym * 'd sym * 'e sym ->
             ('a * 'b * 'c * 'd * 'e) rhs
           val add_rule : grammar ref -> 'a sym -> 'a rhs -> unit
           val add_action : 'a rhs -> ('a -> 'b) -> 'b rhs
         end) ->
    sig
      val example : string
      val g : X.grammar ref
      val add_rule : 'a X.sym -> 'a X.rhs -> unit
      val f :
        star:(sep:([> `Ignore ] as 'a) X.sym ->
              ([> `Rule of
                    ([> `Dq of 'd | `Nt of 'e | `Qu of 'f | `Sq of 'd ] as 'c) *
                    ([> `Rhs of 'h ] as 'g) ]
               as 'b)
              X.sym -> 'i X.sym) ->
        plus:(sep:'a X.sym ->
              ([> `Symsact of 'h * ([> `Code of 'd ] as 'k)
                | `Var_eq of 'l option * 'c ]
               as 'j)
              X.sym -> 'h X.sym) ->
        opt:'m ->
        eps:'n X.sym ->
        header:'o ->
        a:(string -> 'p X.sym) ->
        upto_a:(string -> 'd X.sym) ->
        whitespace_and_comments:'a X.sym ->
        _AZs:'e X.sym ->
        azAZs:'f X.sym ->
        re:(string -> 'l X.sym) ->
        eof:'q X.sym ->
        _GRAMMAR:'i X.sym ->
        _RULES:'i X.sym ->
        _RULE:'b X.sym ->
        _RHS:'g X.sym ->
        _SYMSACT:'j X.sym ->
        _RHSSEP:'a X.sym ->
        _CODE:'k X.sym ->
        _SYMS:'h X.sym ->
        _VAR_EQ_SYM:'j X.sym ->
        _VAR_EQ:[ `None_var | `Some_var of 'l ] X.sym ->
        _SYM:'c X.sym -> _NT:'c X.sym -> _TM:'c X.sym -> unit
    end

*)
