(** Lexer for the CCaTT language. *)

open Parser

let digit = [%sedlex.regexp? '0' .. '9']
let greek = [%sedlex.regexp? Utf8 ("α" | "β" | "γ")]
let first_letter = [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z' | greek]
let letter = [%sedlex.regexp? first_letter | '-' | '+' | digit | '\'']
let ident = [%sedlex.regexp? first_letter, Star letter]
let space = [%sedlex.regexp? ' ' | '\t' | '\r']

let rec token lexbuf =
  match%sedlex lexbuf with
  | "coh" -> COH
  | "ncoh" -> NCOH
  | "let" -> LET
  | "fun" -> FUN
  | '.' -> OBJ
  | '(' -> LPAR
  | ')' -> RPAR
  | '{' -> LACC
  | '}' -> RACC
  | ':' -> COL
  | "->" | Utf8 "→" -> ARR
  | "=>" | Utf8 "⇒" -> HOM
  | '*' | Utf8 "×" -> TIMES
  | '1' -> ONE
  | '=' -> EQ
  | "<=>" | Utf8 "⇔" -> IEQ
  | ":=" -> EQDEF
  | '_' -> HOLE
  | '!' -> OP
  | "~>" | Utf8 "⤳" -> MARR
  | "include \"", Star (Compl '"'), '"' ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    INCLUDE (String.sub s 9 (String.length s - 10))
  | ident -> IDENT (Sedlexing.Utf8.lexeme lexbuf)
  | Plus space -> token lexbuf
  | "#-#", Star (Compl '\n') ->
    let s = Sedlexing.Utf8.lexeme lexbuf in
    SETTING (String.sub s 3 (String.length s - 3))
  | '#', Star (Compl '\n') -> token lexbuf
  | "--", Star (Compl '\n') -> token lexbuf
  | '\n' -> token lexbuf
  | eof -> EOF
  | _ -> failwith (Printf.sprintf "unexpected character %s" (Sedlexing.Utf8.lexeme lexbuf))
