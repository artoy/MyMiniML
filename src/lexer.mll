{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);
  ("rec", Parser.REC);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "&&" { Parser.ANDAND }
| "||" { Parser.BARBAR }
| "(*" { comment 1 lexbuf }
| "=" { Parser.EQ }
| "->" { Parser.RARROW }
| "[]" { Parser.NIL }
| "::" { Parser.APPEND }
| "|" { Parser.BAR }
| "[" { Parser.LBOX }
| "]" { Parser.RBOX }
| ";" { Parser.SEMI }

| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
    }
| eof { exit 0 }
and comment n = parse
| "(*" { comment (n+1) lexbuf }
| "*)" { if (n=1) then (main lexbuf) else (comment (n-1) lexbuf) }
| _ { comment n lexbuf }


