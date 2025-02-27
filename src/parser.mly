%{
(** Parser for the CCaTT language. *)

open Term

let defpos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let mk ?pos e =
  let pos = Option.value ~default:(defpos ()) pos in
  mk ~pos e

let abss ?pos l e =
  let pos = Option.value ~default:(defpos ()) pos in
  abss ~pos l e

let pis ?pos l a =
  let pos = Option.value ~default:(defpos ()) pos in
  pis ~pos l a
%}

%token LET CHECK NCOH FUN TO HOLE
%token COH ARR HOM EQ EQDEF OBJ TIMES ONE
%token LPAR RPAR LACC RACC COL
%token <string> IDENT
%token EOF

%right ARR
%right TO
%right HOM
%right TIMES
%right EQ

%start prog
%type <Term.prog> prog
%%

prog:
  | cmd prog { $1::$2 }
  | EOF { [] }

cmd:
  | COH ident args COL expr { Let ($2, None, abs_coh ~pos:(defpos()) $2 $3 $5) }
  | NCOH ident args COL expr { NCoh ($2, List.map (fun (_,x,a) -> x,a) $3, $5) }
  | LET IDENT args type_opt EQDEF expr { Let ($2, Option.map (pis $3) $4, abss $3 $6) }
  | CHECK expr { Check $2 }

type_opt:
  | COL expr { Some $2 }
  | { None }

expr:
  | FUN args TO expr { abss $2 $4 }
  | expr ARR expr { mk (Arr (hole ~pos:(defpos()) (), $1, $3)) }
  | expr HOM expr { mk (Hom ($1, $3)) }
  | expr EQ expr { if Setting.has_elements () then mk (Id (hole ~pos:(defpos()) (), $1, $3)) else mk (Arr (hole ~pos:(defpos()) (), $1, $3)) }
  | expr EQ LACC expr RACC expr %prec EQ { if Setting.has_elements () then mk (Id ($4, $1, $6)) else mk (Arr ($4, $1, $6)) }
  | expr TIMES expr { mk (Prod ($1, $3)) }
  | ONE { mk One }
  | aexpr { $1 }

aexpr:
  | aexpr sexpr { mk (App (`Explicit, $1, $2)) }
  | aexpr LACC expr RACC { mk (App (`Implicit, $1, $3)) }
  | sexpr { $1 }

/* Simple expression */
sexpr:
  | OBJ { mk Obj }
  | IDENT { mk (Var $1) }
  | HOLE { hole ~pos:(defpos()) ~real:true () }
  | LPAR expr RPAR { $2 }

args:
  | LPAR idents COL expr RPAR args { (List.map (fun x -> `Explicit, x, $4) $2)@$6 }
  | LACC idents COL expr RACC args { (List.map (fun x -> `Implicit, x, $4) $2)@$6 }
  | { [] }

ident:
  | HOLE { "_" }
  | IDENT { $1 }

idents:
  | ident { [$1] }
  | ident idents { $1::$2 }
