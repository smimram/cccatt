{
open Lexing
open Parser

let utf8 ?(n=1) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = pos.pos_bol + n }

(** Number of utf8 characters. *)
let utf8_length s =
  if not (String.is_valid_utf_8 s) then String.length s else
    let rec aux n i =
      if i >= String.length s then n else
        let c = String.get_utf_8_uchar s i in
        aux (n + 1) (i + Uchar.utf_decode_length c)
    in
    aux 0 0

let utf8_advance s lexbuf = utf8 ~n:(String.length s - utf8_length s) lexbuf
}

let space = ' ' | '\t' | '\r'
let first_letter = ['_''a'-'z''A'-'Z'] | "α" | "β" | "γ"
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
  | (first_letter letter* as str) { utf8_advance str lexbuf; IDENT str }
  | space+ { token lexbuf }
  | "#-#"([^'\n']* as s) { Setting.parse s; token lexbuf }
  | "#"[^'\n']* { token lexbuf }
  | "--"[^'\n']* { token lexbuf }
  | "\n" { Lexing.new_line lexbuf; token lexbuf }
  | eof { EOF }
