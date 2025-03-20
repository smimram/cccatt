%{
(** Parser for the CCaTT language. *)
open Term

let mk ~pos e = mk ~pos e
%}

%token LET NCOH FUN TO HOLE
%token COH ARR HOM EQ IEQ EQDEF OBJ TIMES ONE OP
%token LPAR RPAR LACC RACC COL MARR
%token <string> IDENT
%token <string> INCLUDE
%token <string> SETTING
%token EOF

%right ARR
%right TO
%right HOM
%right TIMES
%right EQ IEQ
%nonassoc OP

%start prog
%type <Term.prog> prog
%%

prog:
  | cmd prog { $1::$2 }
  | EOF { [] }

cmd:
  | COH ident args COL expr { Let ($2, None, abs_coh ~pos:$loc $2 $3 $5) }
  | NCOH ident args COL expr { NCoh ($2, List.map (fun (_,x,a) -> x,a) $3, $5) }
  | LET IDENT args type_opt EQDEF expr { Let ($2, Option.map (pis $3) $4, abss $3 $6) }
  | INCLUDE { Include $1 }
  | SETTING { Setting $1 }

type_opt:
  | COL expr { Some $2 }
  | { None }

expr:
  | FUN args TO expr { abss ~pos:$loc $2 $4 }
  | expr ARR expr { mk ~pos:$loc (Arr (hole ~pos:$loc (), $1, $3)) }
  | expr HOM expr { mk ~pos:$loc (Hom ($1, $3)) }
  | expr EQ eqtype expr { mk ~pos:$loc (Arr ($3, $1, $4)) }
  | expr IEQ eqtype expr { mk ~pos:$loc (Id ($3, $1, $4)) }
  | expr TIMES expr { mk ~pos:$loc (Prod ($1, $3)) }
  | OP expr { mk ~pos:$loc (Op $2) }
  | aexpr { $1 }

aexpr:
  | aexpr sexpr { mk ~pos:$loc (App (`Explicit, $1, $2)) }
  | aexpr LACC expr RACC { mk ~pos:$loc (App (`Implicit, $1, $3)) }
  | sexpr { $1 }

/* Simple expression */
sexpr:
  | OBJ { mk ~pos:$loc Obj }
  | IDENT { mk ~pos:$loc (Var $1) }
  | HOLE { hole ~pos:$loc ~real:true () }
  | ONE { mk ~pos:$loc One }
  | LPAR expr RPAR { $2 }

args:
  | LPAR idents COL expr RPAR args { (List.map (fun x -> `Explicit, x, $4) $2)@$6 }
  | LACC idents COL expr RACC args { (List.map (fun x -> `Implicit, x, $4) $2)@$6 }
  | MARR LPAR expr RPAR args { (`Explicit, "_", $3)::$5 }
  | MARR LACC expr RACC args { (`Implicit, "_", $3)::$5 }
  | { [] }

eqtype:
  | LACC expr RACC { $2 }
  | { hole ~pos:$loc () }

ident:
  | HOLE { "_" }
  | IDENT { $1 }

idents:
  | ident { [$1] }
  | ident idents { $1::$2 }
