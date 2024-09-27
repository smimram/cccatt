%{
open Lang

let defpos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let mk ?pos e =
  let pos = Option.value ~default:(defpos ()) pos in
  mk ~pos e

let abss ?pos l e =
  let rec aux = function
    | (x,a)::l -> mk ?pos (Abs (x, a, aux l))
    | [] -> e
  in
  aux l
%}

%token LET CHECK NCOH
%token COH HOM EQ EQDEF OBJ TIMES
%token LPAR RPAR COL
%token <string> IDENT
%token EOF

%right HOM
%right TIMES
%right EQ

%start prog
%type <Lang.prog> prog
%%

prog:
  | cmd prog { $1::$2 }
  | EOF { [] }

cmd:
  | COH IDENT args COL expr { Let ($2, None, mk (Coh ($3, $5))) }
  | NCOH args COL expr { NCoh ($2, $4) }
  | LET IDENT args type_opt EQDEF expr { Let ($2, $4, abss $3 $6) }
  | CHECK expr { Check $2 }

type_opt:
  | COL expr { Some $2 }
  | { None }

expr:
  | expr HOM expr { mk (Hom ($1, $3)) }
  | expr EQ expr { mk (Id (ref None, $1, $3)) }
  | expr TIMES expr { mk (Prod ($1, $3)) }
  | aexpr { $1 }

aexpr:
  | aexpr sexpr { mk (App ($1, $2)) }
  | sexpr { $1 }

/* Simple expression */
sexpr:
  | OBJ { mk Obj }
  | IDENT { mk (Var $1) }
  | LPAR expr RPAR { $2 }

args:
  | LPAR idents COL expr RPAR args { (List.map (fun x -> x, $4) $2)@$6 }
  | { [] }

idents:
  | IDENT { [$1] }
  | IDENT idents { $1::$2 }
