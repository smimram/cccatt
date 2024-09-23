{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'

rule token = parse
  | "coh" { COH }
  | "ncoh" { NCOH }
  | "let" { LET }
  | "*" { STAR }
  | "(" { LPAR }
  | ")" { RPAR }
  | ":" { COL }
  | "->" { HOM }
  | "→" { utf8 ~n:2 lexbuf; HOM }
  | "=" { EQ }
  | ":=" { EQDEF }
  | (['_''a'-'z''A'-'Z']['-''+''a'-'z''A'-'Z''0'-'9''_']*['\'''-''+''!']* as str) { IDENT str }
  | space+ { token lexbuf }
  | "#"[^'\n']* { token lexbuf }
  | "--"[^'\n']* { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
