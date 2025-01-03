(** The CCaTT language. *)

open Extlib
open Common
open Term

exception Unification

(** Make sure that two values are equal (and raise [Unification] if this cannot be the case). *)
(* The first argument is the alpha-conversion to apply to t *)
let rec unify tenv env ?(alpha=[]) t t' =
  (* Printf.printf "unify %s with %s\n%!" (to_string t) (to_string t'); *)
  let unify tenv env ?(alpha=alpha) = unify tenv env ~alpha in
  match t.desc, t'.desc with
  | Var x, Var y ->
    let x = match List.assoc_opt x alpha with Some x -> x | None -> x in
    if x <> y then raise Unification
  | Hom (a, b), Hom (a', b') ->
    unify tenv env a a';
    unify tenv env b b'
  | Obj, Obj -> ()
  | Type, Type -> ()
  | Prod (a, b), Prod (a', b') -> unify tenv env a a'; unify tenv env b b'
  | One, One -> ()
  | Pi (i, x, a, b), Pi (i', x', a', b') ->
    if i <> i' then raise Unification;
    unify tenv env a a';
    let tenv = (x',eval env a)::tenv in
    let env = (x',var x')::env in
    unify tenv env ~alpha:((x,x')::alpha) b b'
  | Meta { value = Some t; _ }, _ -> unify tenv env t t'
  | _, Meta { value = Some t'; _ } -> unify tenv env t t'
  | Meta m, Meta m' when m = m' -> ()
  | Meta m, _ ->
    if has_metavariable m t' then raise Unification;
    let t' = check tenv env t' m.ty in
    m.value <- Some t'
  | _, Meta m' ->
    if has_metavariable m' t then raise Unification;
    let t = check tenv env t m'.ty in
    m'.value <- Some t
  | _ -> raise Unification

(** Evaluate an expression to a value. *)
and eval env e =
  (* Printf.printf "* eval: %s\n" (to_string e); *)
  (* Printf.printf "  env : %s\n" (string_of_context env); *)
  let mk ?(pos=e.pos) = mk ~pos in
  let var ?(pos=e.pos) = var ~pos in
  match e.desc with
  | Coh (l, a) ->
    let l, a =
      let env = ref env in
      let l = List.map (fun (x,a) -> let a = eval !env a in env := (x, var x) :: !env; x, a) l in
      let a = eval !env a in
      l, a
    in
    mk (Coh (l, a))
  | Var x ->
    (
      match List.assoc_opt x env with
      | Some v -> mk v.desc
      | None -> failure e.pos "unexpected error: value for %s not found" x
    )
  | Abs (i,x,a,e) ->
    let x' = if List.mem_assoc x env then fresh_var_name () else x in
    mk (Abs (i,x', eval env a, eval ((x,var x')::env) e))
  | App (i,t,u) ->
    (
      match (eval env t).desc with
      | Abs (i',x,_,t) ->
        if i <> i' then error "Application mismatch in %s (%s instead of %s application)" (to_string e) (string_of_implicit i) (string_of_implicit i');
        let u = eval env u in
        eval ((x,u)::env) t
      | _ -> assert false
    )
  | Pi (i, x, a, b) ->
    let x' = if List.mem_assoc x env then fresh_var_name () else x in
    mk (Pi (i, x', eval env a, eval ((x,var x')::env) b))
  | Id (a, t, u) -> mk (Id (eval env a, eval env t, eval env u))
  | Obj -> mk Obj
  | Hom (a, b) -> mk (Hom (eval env a, eval env b))
  | Prod (a, b) -> mk (Prod (eval env a, eval env b))
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
  | Coh (l, a) ->
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
    Pasting.check ~pos l a;
    mk ~pos (Coh (l, a)), eval env a
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
    (
      let t, a = infer tenv env t in
      match a.desc with
      | Pi (i', x, a, b) when i = i' ->
        let u = check tenv env u a in
        mk ~pos (App (i, t, u)), eval ((x, eval env u)::env) b
      | Pi (`Implicit, _x, _a, _b) when i = `Explicit ->
        let t = mk ~pos:t.pos (App (`Implicit, t, hole ~pos:t.pos ())) in
        infer tenv env (mk ~pos (App (i, t, u)))
      | _ -> failure t.pos "of type %s but a function was expected" (to_string a)
    )
  | Pi (i, x, a, b) ->
    let a = check tenv env a (mk ~pos:a.pos Type) in
    let b = check ((x, eval env a)::tenv) ((x, var ~pos:a.pos x)::env) b (mk ~pos:b.pos Type) in
    mk ~pos (Pi (i, x, a, b)), mk ~pos Type
  | Id (a, t, u) ->
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let a' = eval env a in
    let t = check tenv env t a' in
    let u = check tenv env u a' in
    mk ~pos (Id (a, t, u)), mk ~pos Type
  | Obj ->
    mk ~pos Obj, mk ~pos Type
  | Hom (a, b) ->
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let b = check tenv env b (mk ~pos:b.pos Obj) in
    mk ~pos (Hom (a, b)), mk ~pos Obj
  | Prod (a, b) ->
    let a = check tenv env a (mk ~pos:a.pos Obj) in
    let b = check tenv env b (mk ~pos:b.pos Obj) in
    mk ~pos (Prod (a, b)), mk ~pos Obj
  | One ->
    mk ~pos One, mk ~pos Obj
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
    try if not (b.desc = Obj && a.desc = Type) then unify tenv env b a; e
    with
    | (* Unification *) _ ->
      if is_implicit_pi b && not (is_implicit_pi a) then
        let e = mk ~pos:e.pos (App (`Implicit, e, hole ~pos:e.pos ())) in
        check tenv env e a
      else failure e.pos "got %s but %s expected" (to_string b) (to_string a)

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
         warning "unelaborated ?%d at %s" m.id (Pos.Option.to_string m.source_pos)
    ) (List.sort compare m)

let exec_command (tenv, env) p =
  match p with
  | Let (x, a, e) ->
    (* printf "*** let %s := %s\n%!" x (to_string e); *)
    (* print_endline "inferring"; *)
    let e, a =
      match a with
      | Some a ->
        let m = metavariables a in
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
  | Check e ->
    let e, a = infer tenv env e in
    message "check %s : %s" (Pos.to_string e.pos) (to_string a);
    tenv, env
  | NCoh (l, a) ->
    check tenv env (pis ~pos:a.pos (List.map (fun (x,a) -> `Explicit,x,a) l) a) (mk ~pos:a.pos Type) |> ignore;
    (try Pasting.check ~pos:a.pos l a; failure a.pos "expression accepted as a coherence" with _ -> ());
    tenv, env

(** Execute a program. *)
let exec = List.fold_left exec_command
