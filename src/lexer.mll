{

open Parser

}

let newline = '\n' | "\r\n"
let digit = ['0'-'9']

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | ';' { line_comment lexbuf }
  | digit*'.'?digit+ { NUMBER (float_of_string(Lexing.lexeme lexbuf)) }
  | "#t" { TRUE }
  | "#f" { FALSE }
  | [^'(' ')' '0' - '9' ' ' '\t' '\n' '.'][^' ' '\t' '\n' '(' ')']* {
    SYMBOL(Lexing.lexeme lexbuf)
  }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '.' { DOT }
  | eof { EOF }

and line_comment = parse
  | newline { token lexbuf }
  | _ { line_comment lexbuf }

{}
