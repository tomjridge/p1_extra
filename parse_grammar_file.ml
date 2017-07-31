(* use p1 to parse a grammar file *)

(* example ---------------------------------------------------------- *)

let example = {|

S -> E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}


(* init spec -------------------------------------------------------- *)

let sq (*single quote*) = {|'|}
let dq = {|"|}

let err s = failwith s

(* NOTE this is the grammar of grammars - concrete grammars are
   expressed using the following format *)

(* FIXME add actions on homogeneous lists to following *)
let f 
    ~add_rule ~add_action
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
    let ( --> ) = add_rule in
    let ( ** ) = add_action in
    header(); (* FIXME remove? *)

    _GRAMMAR --> [__;_RULES; __; eof ]  ** (fun [_;rs;_;_] -> rs);
    _RULES --> [star ~sep:__ _RULE] ** (fun [rs] -> rs); 
    _RULE --> [_SYM; __; a "->"; __; _RHS] ** (
        fun [nt;_;_;_;rhs] -> `Rule(nt,rhs));
    _RHS --> [plus ~sep:_RHSSEP _SYMSACT] ** (
        fun x -> `Rhs x);
    _RHSSEP --> [__;a "|";__] ** (fun _ -> `Ignore);

    _SYMSACT --> [_SYMS;__;_CODE] ** (
        function [`List syms;_;`Code code] -> `Symsact (syms,code)
               | _ -> err __LOC__);
    _SYMS --> [plus ~sep:__ _VAR_EQ_SYM] ** (fun [x] -> x);

    _CODE --> [a "{{";upto_a "}}";a "}}"] ** (
        function [_;`String c;_] -> `Code c | _ -> err __LOC__);

    _VAR_EQ_SYM --> [_VAR_EQ; _SYM] ** (
        function | [`Some_var v;x] -> `Var_eq(Some v,x)
                 | [`None_var;x] -> `Var_eq(None,x)
                 | _ -> err __LOC__);
    _VAR_EQ --> [re "[a-z]+";a "="] ** (
        function | [`String v;_] -> `Some_var v | _ -> err __LOC__);
    _VAR_EQ --> [eps] ** (fun _ -> `None_var);

    _SYM --> [_NT] ** (fun [x] -> x);
    _SYM --> [_TM] ** (fun [x] -> x);

    _TM --> [a sq;upto_a sq;a sq] ** (
        function [_;`String s;_] -> `Sq s | _ -> err __LOC__);
    _TM --> [a dq;upto_a dq;a dq] ** (
        function [_;`String s;_] -> `Dq s | _ -> err __LOC__);
    _TM --> [a "?";azAZs;a "?"] ** (
        function [_;`String s;_] -> `Qu s | _ -> err __LOC__);
    _NT --> [_AZs] ** (
        function [`String x] -> `Nt x | _ -> err __LOC__); 
  end

;;


let _ = f

(* now we need to convert this into a parser; nonterminals can be
   identified using strings; then we recursively map the generator
   over the rhs-s; see ridge11cpp, grammar_to_parser; a complication
   is that we are using an imperative interface, so avoid problems
   with let rec and staging of effects such as memoization; perhaps
   just use the above to generate the source code for a p1 parser *)


(* concretization --------------------------------------------------- *)

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


(* grammar_to_parser, with actions ---------------------------------- *)

let f'' () =
  let rs = ref [] in
  let add_rule nt (rhs,act) = 
(*    R(nt,rhs) |> fun r -> 
    r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;*)
    rs:=(nt,rhs,act)::!rs
  in
  let add_action rhs a = (rhs,a) in
  let star ~sep nt = E_star(sep,nt) in
  let plus ~sep nt = E_plus(sep,nt) in
  let opt = P1_combinators.opt in
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
  ignore (f 
            ~add_rule ~add_action
            ~star ~plus ~opt ~eps:(a "")
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

let _ = 
  let rs = f'' () in
  let _ = rs in
  ()

(* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
let grammar_to_parser' ~rules = 
  let open P1_core in
  let open P1_combinators in
  let open P1_terminals in
  let rec nt_to_parser' nt = 
    let seq_list act xs = seq_list xs >> act in
    let alt_list xs = alt_list xs >> fun xs -> xs in 
    rules |> List.filter (fun (e,_,_) -> e=nt) |> fun rs ->
    alt_list (
      rs 
      |> List.map (
        fun (_,rhs,act) -> rhs |> List.map elt_to_parser |> seq_list act))
  and nt_to_parser nt = nt_to_parser' nt >> fun x -> x
  and tm_to_parser' = function
    | A s -> a s
    | Upto_a s -> upto_a s
    | Ws -> ws
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
    | E_TM tm -> tm_to_parser tm
    | E_NT nt -> nt_to_parser (nt_to_string nt)
  in
  nt_to_parser

let grammar_to_parser = 
  grammar_to_parser' 
    ~rules:(
      f'' ()
      |> List.map (function 
          | (E_NT e,es,act) -> (nt_to_string e,es,act) 
          | _ -> failwith __LOC__))


let _ = grammar_to_parser



(* pretty-print ----------------------------------------------------- *)


(* FIXME move elsewhere *)
let rec pp_rule = function
  | `Rule(`Nt nt,`Rhs[`List sas]) -> (nt,List.map pp_sas sas)
  | _ -> err __LOC__
and pp_sas = function
  | `Symsact (syms,code) -> (List.map pp_sym syms,code)
  | _ -> err __LOC__
and pp_sym' = function
    | `Nt s -> `Nt s
    | `Dq s -> `Dq s
    | `Qu s -> `Qu s
    | `Sq s -> `Sq s
    | _ -> err __LOC__
and pp_sym = function
  | `Var_eq (None,x) -> (`Elt(None,x |> pp_sym'))
  | `Var_eq (Some v,x) -> (`Elt(Some v,x |> pp_sym'))
  | _ -> err __LOC__
and pp_rules = function
  | `List rs -> List.map pp_rule rs
  | _ -> (err __LOC__)


(* improve typing --------------------------------------------------- *)

module Improved_typing = struct
  type sym = [ `Nt of string | `Qu of string | `Sq of string | `Dq of string ]

  type elt = [ `Elt of (string option * sym) ]

  type rule = {nt:string; rhs: (elt list * string) list }
end

open Improved_typing

let parse_grammar_file s = 
  s
  |> P1_core.run_parser (grammar_to_parser "Grammar") 
  |> (fun [x] -> x)
  |> pp_rules
  |> List.map (fun (nt,rhs) -> {nt;rhs})
