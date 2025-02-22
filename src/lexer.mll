{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }
}

let space = ' ' | '\t' | '\r'
let first_letter = ['_''a'-'z''A'-'Z'] | "α" | "β"
let letter = first_letter | ['-''+''0'-'9''\'']

rule token = parse
  | "coh" { COH }
  | "ncoh" { NCOH }
  | "let" { LET }
  | "fun" { FUN }
  | "check" { CHECK }
  | "." { OBJ }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LACC }
  | "}" { RACC }
  | ":" { COL }
  | "->" { ARR }
  | "→" { utf8 ~n:2 lexbuf; ARR }
  | "=>" { HOM }
  | "⇒" { utf8 ~n:2 lexbuf; HOM }
  | "*" { utf8 lexbuf; TIMES }
  | "×" { utf8 lexbuf; TIMES }
  | "1" { ONE }
  | "=" { EQ }
  | ":=" { EQDEF }
  | "_" { HOLE }
  | (first_letter letter* as str) { IDENT str }
  | space+ { token lexbuf }
  | "#-#"([^'\n']* as s) { Setting.parse s; token lexbuf }
  | "#"[^'\n']* { token lexbuf }
  | "--"[^'\n']* { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
