(*
 * lexer.mll
 *
 *     bogoscheme Lexer.
 *
 *)

{
open Parser
}

(* Some useful definitions. *)
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_chars   = ['a' - 'z' '+' '-' '*' '/' '=' '<' '>' '!']
let comment    = ';'[^'\n']*

(* The lexer definition itself. *)
rule lex = parse
  | comment       { lex lexbuf }
  | whitespace    { lex lexbuf }
  | '('           { TOK_LPAREN }
  | ')'           { TOK_RPAREN }
  | "#u"          { TOK_UNIT }
  | "#t"          { TOK_BOOL true }
  | "#f"          { TOK_BOOL false }
  | integer as i  { TOK_INT (int_of_string i) }
  | id_chars+ as s { TOK_ID s }
  | eof           { TOK_EOF }

  (* lexer error -- this should never happen *)
  | _          { 
      raise (Failure ("unrecognized token: " ^ (Lexing.lexeme lexbuf))) 
    }

{
(* Nothing. *)
}

      
