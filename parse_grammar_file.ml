(* use p1 to parse a grammar file *)

let example = {|

S -> E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}


let sq (*single quote*) = {|'|}
let dq = {|"|}


let f 
    ~add ~star ~plus ~header
    (* terminals *)
    ~a
    ~upto_a  
    ~whitespace_and_comments  (* whitespace and comments *)
    ~_AZs ~azAZs
    ~eof 
    (* nonterminals *)
    ~_GRAMMAR ~_RULES ~_RULE 
    ~_RHS ~_SYMS ~_SYM ~_SYMSACT ~_CODE ~_RHSSEP
    ~_NT ~_TM
  =
  begin
    let __ = whitespace_and_comments in
    let ( --> ) = add in

    header();

    _GRAMMAR --> [_RULES; __; eof ];
    _RULES --> [star ~sep:__ _RULE];
    _RULE --> [_SYM; __; a "->"; __; _RHS];

    _RHS --> [plus ~sep:_RHSSEP _SYMSACT];
    _RHSSEP --> [__;a "|";__];

    _SYMSACT --> [_SYMS;__;_CODE];
    _SYMS --> [plus ~sep:__ _SYM];

    _CODE --> [a "{{";upto_a "}}";a "}}"];

    _SYM --> [_NT];
    _SYM --> [_TM];

    _TM --> [a sq;upto_a sq;a sq];
    _TM --> [a dq;upto_a dq;a dq];
    _NT --> [_AZs];
    _NT --> [a "?";azAZs;a "?"]
  end

let _ = f

(* now we need to convert this into a parser; nonterminals can be
   identified using strings; then we recursively map the generator
   over the rhs-s; see ridge11cpp, grammar_to_parser; a complication
   is that we are using an imperative interface, so avoid problems
   with let rec and staging of effects such as memoization; perhaps
   just use the above to generate the source code for a p1 parser *)


type elt = 
  | Star of elt * elt | Plus of elt * elt | A of string | Upto_a of string 
  | Ws | AZs | AZazs | Eof | Grammar | Rules | Rule | Rhs | Syms | Sym | Symsact 
  | Code | Rhssep | NT | TM [@@deriving yojson]

type rule = R of (elt * elt list) [@@deriving yojson]

let f' () =
  let rs = ref [] in
  let add nt rhs = 
    R(nt,rhs) |> fun r -> 
    r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
    rs:=r::!rs
  in
  let star ~sep nt = Star(sep,nt) in
  let plus ~sep nt = Plus(sep,nt) in
  let header () = () in
  let a s = A s in
  let upto_a s = Upto_a s in
  let whitespace_and_comments = Ws in
  let _AZs = AZs in
  let azAZs = AZazs in
  let eof = Eof in
  let _GRAMMAR = Grammar in
  let _RULES = Rules in
  let _RULE = Rule in
  let _RHS = Rhs in
  let _SYMS = Syms in
  let _SYM = Sym in
  let _SYMSACT = Symsact in
  let _CODE = Code in
  let _RHSSEP = Rhssep in
  let _NT = NT in
  let _TM = TM in
  f 
    ~add ~star ~plus ~header
    (* terminals *)
    ~a
    ~upto_a  
    ~whitespace_and_comments  (* whitespace and comments *)
    ~_AZs ~azAZs
    ~eof 
    (* nonterminals *)
    ~_GRAMMAR ~_RULES ~_RULE 
    ~_RHS ~_SYMS ~_SYM ~_SYMSACT ~_CODE ~_RHSSEP
    ~_NT ~_TM;
  List.rev !rs

let rs = f' ()

let rs = rs |> List.map (function R(e,es) -> (e,es))

let nts = rs |> List.map (fun (e,es) -> e::es) |> List.concat |> Tjr_list.unique

let lhs = rs |> List.map (fun (e,es) -> e) |> Tjr_list.unique

(* for each lhs nt, extract the rules and pretty print *)

(* hacky way to convert to string *)
let rec elt_to_string elt = 
  match elt with
  | Star(sep,x) -> 
    {| (star ~sep:$sep $x) |} 
    |> Tjr_string.replace_list
         ~subs:["$sep",elt_to_string sep; "$x",elt_to_string x]
  | Plus(sep,x) -> 
    {| (plus ~sep:$sep $x) |} 
    |> Tjr_string.replace_list
         ~subs:["$sep",elt_to_string sep; "$x",elt_to_string x]
  | A(s) -> 
    {outer| a {|$s|}  |outer} 
    |> Tjr_string.replace_list
         ~subs:["$s",s]
  | Upto_a(s) -> 
    {outer| upto_a {|$s|}  |outer} 
    |> Tjr_string.replace_list
         ~subs:["$s",s]
  | _ -> 
    elt |> elt_to_yojson |> function `List (`String s::_) -> s | _ -> failwith __LOC__

let _ = Star(Ws,Rules) |> elt_to_yojson

let rs' = rs |> List.map (fun (e,es) -> elt_to_string e,List.map elt_to_string es)

let rs'' = rs' |> List.map (fun (e,es) -> (e, Tjr_string.concat_strings ~sep:" **> " es))

(*

val rs'' : (string * string) list =
  [("Grammar", "Rules **> Ws **> Eof"); ("Rules", " (star ~sep:Ws Rule) ");
   ("Rule", "Sym **> Ws **>  a {|->|}   **> Ws **> Rhs");
   ("Rhs", " (plus ~sep:Rhssep Symsact) ");
   ("Rhssep", "Ws **>  a {|||}   **> Ws");
   ("Symsact", "Syms **> Ws **> Code"); ("Syms", " (plus ~sep:Ws Sym) ");
   ("Code", " a {|{{|}   **>  upto_a {|}}|}   **>  a {|}}|}  ");
   ("Sym", "NT"); ("Sym", "TM");
   ("TM", " a {|'|}   **>  upto_a {|'|}   **>  a {|'|}  ");
   ("TM", " a {|\"|}   **>  upto_a {|\"|}   **>  a {|\"|}  "); ("NT", "AZs");
   ("NT", " a {|?|}   **> AZazs **>  a {|?|}  ")]

*)


(* FIXME maybe quicker just to use grammar_to_parser on the rs? *)


(*
let mk_clause nt = 
  {| and $fname = $rhs |}
  |> Tjr_string.replace_first ~sub:"$fname" ~rep:(elt_to_string nt)
  |> fun and_clause ->
  let f rhs =  rhs |> List.map elt_to_string |> Tjr_string.concat_strings ~sep: "**>" in
  let (_,rhs)::_ = rs |> List.filter (fun (e,es) -> e = nt) in
  and_clause |> Tjr_string.replace_first ~sub:"$rhs" ~rep:(f rhs)

let _ = mk_clause NT
*)

(* FIXME this is a bit fiddly; perhaps better to work with just tree rewriting? *)
