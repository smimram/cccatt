%{
open Lang

let defpos () = Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()

let mk ?pos e =
  let pos = Option.value ~default:(defpos ()) pos in
  mk ~pos e

let abss ?pos l e =
  let rec aux = function
    | (x,a)::l -> mk ?pos (Abs ((x,a),aux l))
    | [] -> e
  in
  aux l
%}

%token LET CHECK NCOH
%token COH HOM EQ EQDEF STAR
%token LPAR RPAR COL
%token <string> IDENT
%token EOF

%right HOM
%right EQ

%start prog
%type <Lang.prog> prog
%%

prog:
  | cmd prog { $1::$2 }
  | EOF { [] }

cmd:
  | COH IDENT args COL ty { Let ($2, mk (Coh ($3, $5))) }
  | NCOH args COL ty { NCoh ($2, $4) }
  | LET IDENT args EQDEF expr { Let ($2, abss $3 $5) }
  | CHECK expr { Check $2 }

ty:
  | LPAR ty RPAR { $2 }
  | STAR { mk Obj }
  | IDENT { mk (Var $1) }
  | ty HOM ty { mk (Hom ($1, $3)) }
  | ty EQ ty { mk (Id ($1, $3)) }

expr:
  | aexpr { $1 }

aexpr:
  | aexpr sexpr { mk (App ($1, $2)) }
  | sexpr { $1 }

sexpr:
  | IDENT { mk (Var $1) }
  | LPAR expr RPAR { $2 }

args:
  | LPAR idents COL ty RPAR args { (List.map (fun x -> x, $4) $2)@$6 }
  | { [] }

idents:
  | IDENT { [$1] }
  | IDENT idents { $1::$2 }
