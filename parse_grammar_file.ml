(* use p1 to parse a grammar file *)

let example = {|

S -> E ?ws? ?eof? {{ print_endline (x1 |> string_of_int) }}

E -> x=E y=E z=E {{ x+y+z }}
| "1"  {{ 1 }}
| ""   {{ 0 }}

|}


let sq (*single quote*) = {|'|}
let dq = {|"|}

(* FIXME add actions on homogeneous lists to following *)
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

    header(); (* FIXME remove? *)

    _GRAMMAR --> [__;_RULES; __; eof ];
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
    _TM --> [a "?";azAZs;a "?"];
    _NT --> [_AZs];
  end

let _ = f

(* now we need to convert this into a parser; nonterminals can be
   identified using strings; then we recursively map the generator
   over the rhs-s; see ridge11cpp, grammar_to_parser; a complication
   is that we are using an imperative interface, so avoid problems
   with let rec and staging of effects such as memoization; perhaps
   just use the above to generate the source code for a p1 parser *)


(* NOTE tm' and nt' are specific to the grammar; elt is generic over these *)
type tm' = A of string | Upto_a of string | Ws | AZs | AZazs | Eof 
[@@deriving yojson]

type nt' = 
  | Grammar | Rules | Rule | Rhs | Syms | Sym | Symsact 
  | Code | Rhssep | NT | TM  [@@deriving yojson]

type elt = 
  | E_star of elt * elt | E_plus of elt * elt | E_NT of nt' 
  | E_TM of tm' [@@deriving yojson]

type rule = R of (elt * elt list) [@@deriving yojson]

let f' () =
  let rs = ref [] in
  let add nt rhs = 
    R(nt,rhs) |> fun r -> 
    r |> rule_to_yojson |> Yojson.Safe.pretty_to_string |> print_endline;
    rs:=r::!rs
  in
  let star ~sep nt = E_star(sep,nt) in
  let plus ~sep nt = E_plus(sep,nt) in
  let header () = () in
  let a s = E_TM(A s) in
  let upto_a s = E_TM(Upto_a s) in
  let whitespace_and_comments = E_TM Ws in
  let _AZs = E_TM AZs in
  let azAZs = E_TM AZazs in
  let eof = E_TM Eof in
  let _GRAMMAR = E_NT Grammar in
  let _RULES = E_NT Rules in
  let _RULE = E_NT Rule in
  let _RHS = E_NT Rhs in
  let _SYMS = E_NT Syms in
  let _SYM = E_NT Sym in
  let _SYMSACT = E_NT Symsact in
  let _CODE = E_NT Code in
  let _RHSSEP = E_NT Rhssep in
  let _NT = E_NT NT in
  let _TM = E_NT TM in  (* TM is a nonterminal - it expands to 'x' etc *)
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

let nts = 
  rs |> List.map (fun (e,es) -> e::es) |> List.concat |> Tjr_list.unique

let lhs = rs |> List.map (fun (e,es) -> e) |> Tjr_list.unique

let nt_to_string nt' = 
  nt' |> nt'_to_yojson 
  |> function `List (`String s::_) -> s | _ -> (failwith __LOC__)


(* grammar_to_parser ------------------------------------------------ *)


(* NOTE the following is more-or-less independent of the nature of the
   terminals or the nonterminals *)
let grammar_to_parser' ~rules = 
  let open P1_core in
  let open P1_combinators in
  let open P1_terminals in
  let rec nt_to_parser' nt = 
    let seq_list xs = seq_list xs >> fun xs -> `Seq_list xs in
    let alt_list xs = alt_list xs >> fun xs -> `Alt_list xs in 
    rules |> List.filter (fun (e,_) -> e=nt) |> fun rs ->
    alt_list (
      rs |> List.map @@ fun r -> r |> snd |> List.map elt_to_parser |> seq_list)
  and nt_to_parser nt = nt_to_parser' nt >> fun x -> `NT(nt,x)
  and tm_to_parser' = function
    | A s -> a s
    | Upto_a s -> upto_a s
    | Ws -> ws
    | AZs -> _AZs
    | AZazs -> _AZazs
    | Eof -> eof >> fun _ -> ""
  and tm_to_parser x = tm_to_parser' x >> fun x -> `String x
  and elt_to_parser = function
    | E_star(sep,elt) -> 
      star ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `Star xs
    | E_plus(sep,elt) -> 
      plus ~sep:(elt_to_parser sep) (elt_to_parser elt) >> fun xs -> `Plus xs
    | E_TM tm -> tm_to_parser tm
    | E_NT nt -> nt_to_parser (nt_to_string nt)
  in
  nt_to_parser
  
let grammar_to_parser = 
  grammar_to_parser' 
    ~rules:(
      rs |> List.map @@ 
      function (E_NT e,es) -> (nt_to_string e,es) | _ -> failwith __LOC__)



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


(* string conversion ------------------------------------------------ *)


(* for each lhs nt, extract the rules and pretty print *)

(* hacky way to convert to string *)
let rec elt_to_string elt = 
  match elt with
  | E_star(sep,x) -> 
    {| (star ~sep:$sep $x) |} 
    |> Tjr_string.replace_list
         ~subs:["$sep",elt_to_string sep; "$x",elt_to_string x]
  | E_plus(sep,x) -> 
    {| (plus ~sep:$sep $x) |} 
    |> Tjr_string.replace_list
         ~subs:["$sep",elt_to_string sep; "$x",elt_to_string x]
  | E_TM A(s) -> 
    {outer| a {|$s|}  |outer} 
    |> Tjr_string.replace_list
         ~subs:["$s",s]
  | E_TM Upto_a(s) -> 
    {outer| upto_a {|$s|}  |outer} 
    |> Tjr_string.replace_list
         ~subs:["$s",s]
  | _ -> 
    elt |> elt_to_yojson 
    |> function `List (`String s::_) -> s | _ -> (failwith __LOC__)

let _ = E_star(E_TM Ws,E_NT Rules) |> elt_to_yojson

let rs' = 
  rs |> List.map (fun (e,es) -> elt_to_string e,List.map elt_to_string es)

let rs'' = 
  rs' |> List.map (fun (e,es) -> (e, Tjr_string.concat_strings ~sep:" **> " es))

