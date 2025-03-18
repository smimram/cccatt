(** The CCaTT language. *)

open Extlib
open Common
open Term

exception Unification
exception Type_error of Pos.t * t * t (* at pos got type a instead of b *)
  
(** Make sure that two values are equal (and raise [Unification] if this cannot be the case). *)
(* The first argument is the alpha-conversion to apply to t *)
let rec unify tenv env ?(alpha=[]) t t' =
  (* Printf.printf "unify %s with %s\n%!" (to_string t) (to_string t'); *)
  let unify tenv env ?(alpha=alpha) = unify tenv env ~alpha in
  match t.desc, t'.desc with
  | Var x, Var y ->
    let x = match List.assoc_opt x alpha with Some x -> x | None -> x in
    if x <> y then raise Unification
  | Obj, Obj -> ()
  | Type, Type -> ()
  | Arr (a, t, u), Arr (a', t', u') -> unify tenv env a a'; unify tenv env t t'; unify tenv env u u'
  | Hom (a, b), Hom (a', b') -> unify tenv env a a'; unify tenv env b b'
  | Prod (a, b), Prod (a', b') -> unify tenv env a a'; unify tenv env b b'
  | One, One -> ()
  | Op a, Op a' -> unify tenv env a a'
  | Pi (i, x, a, b), Pi (i', x', a', b') ->
    if i <> i' then raise Unification;
    unify tenv env a a';
    let tenv = (x',eval env a)::tenv in
    let env = (x',var x')::env in
    unify tenv env ~alpha:((x,x')::alpha) b b'
  | Coh (n, l, a, s), Coh (n', l', a', s') ->
    (* TODO: we could also identify same coherences with different names *)
    if n <> n' then raise Unification;
    if List.length l <> List.length l' then raise Unification;
    (* TODO: take the beginning of the context in account in tenv? *)
    List.iter2 (fun (i,x,a) (i',x',a') -> if i <> i' || x <> x' then raise Unification; unify tenv env a a') l l';
    unify tenv env a a';
    if List.length s <> List.length s' then raise Unification;
    List.iter2 (fun (x,t) (x',t') -> if x <> x' then raise Unification; unify tenv env t t') s s'
  | Meta { value = Some t; _ }, _ -> unify tenv env t t'
  | _, Meta { value = Some t'; _ } -> unify tenv env t t'
  | Meta m, Meta m' when m = m' -> ()
  | Meta m, _ ->
    if has_metavariable m t' then raise Unification;
    let t' = check tenv env t' m.ty in
    (* printf "metavariable ?%d gets %s\n" m.id (to_string t); *)
    m.value <- Some t'
  | _, Meta m' ->
    (* print_endline "** term with meta"; *)
    if has_metavariable m' t then raise Unification;
    (* print_endline "** no loop"; *)
    (* printf "check %s : %s\n%!" (to_string t) (to_string m'.ty); *)
    let t = check tenv env t m'.ty in
    (* printf "metavariable ?%d gets %s\n" m'.id (to_string t); *)
    m'.value <- Some t
  | _ -> raise Unification

(** Evaluate an expression to a value. *)
and eval env e =
  (* Printf.printf "* eval: %s\n" (to_string e); *)
  (* Printf.printf "  env : %s\n" (string_of_context env); *)
  let mk ?(pos=e.pos) = mk ~pos in
  let var ?(pos=e.pos) = var ~pos in
  match e.desc with
  | Coh (n, l, a, s) ->
    let l, a =
      let env = ref env in
      let l = List.map (fun (i,x,a) -> let a = eval !env a in env := (x, var x) :: !env; i,x,a) l in
      let a = eval !env a in
      l, a
    in
    let s = List.map (fun (x,t) -> x, eval env t) s in
    mk (Coh (n, l, a, s))
  | Var x ->
    (
      match List.assoc_opt x env with
      | Some v -> mk v.desc
      | None -> mk (Var x)
      (* failure e.pos "unexpected error: value for %s not found" x *)
    )
  | Pi (i, x, a, b) ->
    let x' = if List.mem_assoc x env then fresh_var_name x else x in
    mk (Pi (i, x', eval env a, eval ((x,var x')::env) b))
  | Abs (i,x,a,e) ->
    let x' = if List.mem_assoc x env then fresh_var_name x else x in
    mk (Abs (i,x', eval env a, eval ((x,var x')::env) e))
  | App (i,t,u) ->
    (
      match (eval env t).desc with
      | Abs (i',x,_,t) ->
        if i <> i' then
          error ~pos:e.pos "application mismatch in %s (%s instead of %s application)" (to_string e) (string_of_icit i) (string_of_icit i');
        let u = eval env u in
        eval ((x,u)::env) t
      | _ -> assert false
    )
  | Obj -> mk Obj
  | Id (a, t, u) -> mk (Id (eval env a, eval env t, eval env u))
  | Arr (a, t, u) -> mk (Arr (eval env a, eval env t, eval env u))
  | Hom (a, b) -> mk (Hom (eval env a, eval env b))
  | Prod (a, b) -> mk (Prod (eval env a, eval env b))
  | Op a -> mk (Op (eval env a))
  | One -> mk One
  | Type -> mk Type
  | Meta { value = Some t; _ } -> eval env t
  | Meta { value = None; _ } -> e

(** Infer the type of an expression, elaborates the term along the way. *)
(* NOTE: in the following environments only contain values, and type inference produces values. *)
and infer tenv env (e:Term.t) =
  let pos = e.pos in
  (* printf "* infer %s\n%!" (to_string e); *)
  (* printf "  tenv : %s\n%!" (string_of_context tenv); *)
  (* printf "  env : %s\n%!" (string_of_context env); *)
  (* printf "\n"; *)
  match e.desc with
  | Coh (n, l, a, s) ->
    (* NOTE: we don't check s because we always start with the identity substitution and trust invariants *)
    let env' = s@env in
    let l, a =
      let l' = ref [] in
      let rec aux tenv env = function
        | (i,x,a)::l ->
          let a = check tenv env a (mk ~pos:a.pos Type) in
          l' := (i,x,a) :: !l';
          aux ((x,eval env a)::tenv) ((x,var ~pos:a.pos x)::env) l
        | [] -> check tenv env a (mk ~pos:a.pos Type)
      in
      let a = aux tenv env' l in
      List.rev !l', a
    in
    Pasting.check ~pos (List.map (fun (_,x,a) -> x,a) l) a;
    mk ~pos (Coh (n, l, a, s)), eval env' a
  | Var x ->
    (
      match List.assoc_opt x tenv with
      | Some a -> var ~pos x, a
      | None -> failure e.pos "unknown variable %s" x
    )
  | Abs (i,x,a,t) ->
    let a = check tenv env a (mk ~pos:a.pos Type) in
    let t, b = infer ((x,eval env a)::tenv) ((x, var ~pos:a.pos x)::env) t in
    mk ~pos (Abs (i,x,a,t)), mk ~pos (Pi (i, x, a, b))
  | App (i, t, u) ->
    let t, a =
      match i with
      | `Implicit ->
        infer tenv env t
      | `Explicit ->
        let t, a = infer tenv env t in
        insert tenv env t a
    in
    (
      match a.desc with
      | Pi (i', x, a, b) when i = i' ->
        let u = check tenv env u a in
        mk ~pos (App (i, t, u)), eval ((x, eval env u)::env) b
      | _ -> failure t.pos "of type %s but a function was expected" (to_string a)
    )
  | Pi (i, x, a, b) ->
    let a = check tenv env a (mk ~pos:a.pos Type) in
    let b = check ((x, eval env a)::tenv) ((x, var ~pos:a.pos x)::env) b (mk ~pos:b.pos Type) in
    mk ~pos (Pi (i, x, a, b)), mk ~pos Type
  | Obj ->
    mk ~pos Obj, mk ~pos Type
  | Id (a, t, u) ->
    (* TODO: this should be merged as a higher-dimensional Hom *)
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let a' = eval env a in
    let t = check tenv env t a' in
    let u = check tenv env u a' in
    mk ~pos (Id (a, t, u)), mk ~pos Type
  | Arr (a, t, u) ->
    (* TODO: change this! *)
    let a = check tenv env a (mk ~pos:a.pos Type) in
    let a' = eval env a in
    let t = check tenv env t a' in
    let u = check tenv env u a' in
    mk ~pos (Arr (a, t, u)), mk ~pos Type
  | Hom (a, b) ->
    if not (Setting.has_hom ()) then failure e.pos "internal hom not allowed in this mode";
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let b = check tenv env b (mk ~pos:b.pos Obj) in
    mk ~pos (Hom (a, b)), mk ~pos Obj
  | Prod (a, b) ->
    if not (Setting.has_prod ()) then failure e.pos "products not allowed in this mode";
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let b = check tenv env b (mk ~pos:b.pos Obj) in
    mk ~pos (Prod (a, b)), mk ~pos Obj
  | One ->
    if not (Setting.has_one ()) then failure e.pos "unit not allowed in this mode";
    mk ~pos One, mk ~pos Obj
  | Op a ->
    if not (Setting.has_op ()) then failure e.pos "duals not allowed in this mode";
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    mk ~pos (Op a), mk ~pos Obj
  | Type ->
    mk ~pos Type, mk ~pos Type
  | Meta m ->
    mk ~pos (Meta m), eval env m.ty

(* NOTE: a is supposed to be a value *)
and check tenv env e a =
  (* printf "* check %s : %s\n%!" (to_string e) (to_string a); *)
  match e.desc, a.desc with
  (* Allow casting under implicit arrows. *)
  | Abs(`Implicit,x,a,t), Pi(`Implicit,x',a',b) when x = x' (* TODO: alpha? *) ->
    unify tenv env a a';
    let t = check ((x,a)::tenv) ((x,var x)::env) t b in
    mk ~pos:e.pos (Abs(`Implicit,x,a,t))
  | _ ->
    let e, b = infer tenv env e in
    let e, b = insert tenv env e b in
    if not (Setting.has_elements ()) || not (b.desc = Obj && a.desc = Type) then
      (
        try unify tenv env b a
        with Unification -> raise (Type_error (e.pos, b, a))
      );
    e

(** Given a term of given type, apply all implicit arguments to metavariables. *)
and insert tenv env t a =
  match (unmeta a).desc with
  | Pi (`Implicit,x,a,b) ->
    let u = hole ~pos:t.pos () in
    let u = check tenv env u a in
    let t = mk ~pos:t.pos (App (`Implicit, t, u)) in
    let b = eval ((x, eval env u)::env) b in
    insert tenv env t b
  | _ -> t, a

let print_metavariables_elaboration m =
  List.iter
    (fun (m:meta) ->
       if m.source_pos <> None then
         let v =
           match m.value with
           | Some v -> to_string v
           | None -> "?"
         in
         info "at %s, ?%d elaborated to %s" (Pos.to_string (Option.get m.source_pos)) m.id v
    ) (List.sort compare m)

let print_unelaborated_metavariables m =
  List.iter
    (fun (m:meta) ->
       if m.value = None then
         warning "unelaborated ?%d at %s of type %s" m.id (Pos.Option.to_string m.source_pos) (to_string m.ty)
    ) (List.sort compare m)

(** Parse a string. *)
let parse ?filename s =
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Option.value ~default:"" filename };
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
  | Parsing.Parse_error
  | Parser.Error ->
    let pos = (Lexing.lexeme_end_p lexbuf) in
    Common.error
      "parsing error in file %s at word \"%s\", line %d, character %d"
      pos.Lexing.pos_fname
      (Lexing.lexeme lexbuf)
      pos.Lexing.pos_lnum
      (pos.Lexing.pos_cnum - pos.Lexing.pos_bol - 1)

(** Parse a file. *)
let parse_file f =
  let sin =
    let fi = open_in f in
    let flen = in_channel_length fi in
    let buf = Bytes.create flen in
    really_input fi buf 0 flen;
    close_in fi;
    buf
  in
  parse ~filename:f (Bytes.to_string sin)

let rec exec_command (tenv, env) p =
  match p with
  | Let (x, a, e) ->
    (* printf "*** let %s : %s := %s\n%!" x (Option.value ~default:"?" @@ Option.map to_string a) (to_string e); *)
    (* print_endline "inferring"; *)
    let e, a =
      match a with
      | Some a ->
        let m = metavariables a in
        let a = check tenv env a (mk Type) in
        let a = eval env a in
        print_metavariables_elaboration m;
        let e = check tenv env e a in
        e, a
      | None ->
        infer tenv env e
    in
    let m = metavariables e in
    (* print_endline "checking"; *)
    let v = eval env e in
    print_metavariables_elaboration m;
    print_unelaborated_metavariables m;
    let tenv = (x,a)::tenv in
    let env = (x,v)::env in
    message "defined %s : %s" x (to_string a);
    (* printf "      %s\n%!" (to_string v); *)
    tenv, env
  | NCoh (x, l, a) ->
    check tenv env (pis ~pos:a.pos (List.map (fun (x,a) -> `Explicit,x,a) l) a) (mk ~pos:a.pos Type) |> ignore;
    (
      try
        let l, a =
          let l' = ref [] in
          let rec aux tenv env = function
            | (x,a)::l ->
              let a = check tenv env a (mk ~pos:a.pos Type) in
              l' := (x,a) :: !l';
              aux ((x,eval env a)::tenv) ((x,var ~pos:a.pos x)::env) l
            | [] -> check tenv env a (mk ~pos:a.pos Type)
          in
          let a = aux tenv env l in
          List.rev !l', a
        in
        Pasting.check ~pos:a.pos l a; failure a.pos "expression accepted as a coherence"
      with _ -> ()
    );
    message "not a coherence %s : %s" x (to_string @@ pis_explicit l a);
    tenv, env
  | Include fname ->
    let fname =
      if Sys.file_exists fname then fname
      else if Sys.file_exists (fname^".cccatt") then fname^".cccatt"
      else fname
    in
    Setting.save ();
    let env = exec (tenv,env) (parse_file fname) in
    Setting.restore ();
    env

(** Execute a program. *)
and exec env cmd =
  try List.fold_left exec_command env cmd
  with Type_error (pos, a, b) -> failure pos "got %s but %s expected" (to_string a) (to_string b)
