open Parse_grammar_file_with_more_typing2

let _GRAMMAR = _GRAMMAR

let grammar_to_parser' = grammar_to_parser'

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
