(** Prover. *)

(** Parse a string. *)
let parse s =
  let lexbuf = Lexing.from_string s in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Failure s when s = "lexing: empty token" ->
    let pos = Lexing.lexeme_end_p lexbuf in
    Common.error
      "lexing error in file %s at line %d, character %d"
      pos.Lexing.pos_fname
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
  | Parsing.Parse_error ->
    let pos = (Lexing.lexeme_end_p lexbuf) in
    Common.error
      "parsing error in file %s at word \"%s\", line %d, character %d"
      pos.Lexing.pos_fname
      (Lexing.lexeme lexbuf)
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)

let parse_file f =
  let sin =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = Bytes.create flen in
    really_input fi buf 0 flen;
    close_in fi;
    buf
  in
  parse (Bytes.to_string sin)
