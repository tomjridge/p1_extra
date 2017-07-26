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
