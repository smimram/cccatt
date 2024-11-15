(** The CCaTT language. *)

open Extlib
open Common

let failure pos fmt =
  Printf.ksprintf (fun s -> failwith "%s: %s" (Pos.to_string pos) s) fmt

(** An expression. *)
type t =
  {
    desc : desc;
    pos : Pos.t;
  }

and desc =
  | Coh  of context * t (** coherence *)
  | Var  of string (** variable *)
  | Abs  of implicit * string * t * t (** abstraction *)
  | App  of implicit * t * t (** application *)
  | Pi   of implicit * string * t * t (** Π-type *)
  | Hom  of t * t (** hom type *)
  | Prod of t * t (** product type *)
  | One (** terminal type *)
  | Id   of t * t * t (** identity type *)
  | Meta of meta (** a variable to be unified *)
  | Obj  (** object type *)
  | Type (** the type of types *)

and implicit = [`Explicit |  `Implicit]

and context = (string * t) list

and meta =
  {
    id : int;
    pos : Pos.t option;
    mutable value : t option;
    ty : t;
  }

(** String representation of an expression. This should mostly be useful for debugging (we want to print values). *)
let rec to_string ?(pa=false) e =
  let pa s = if pa then "(" ^ s ^ ")" else s in
  match e.desc with
  (* | Coh (l, a) -> Printf.sprintf "coh[%s|%s]" (List.map (fun (x,a) -> Printf.sprintf "%s:%s" x (to_string a)) l |> String.concat ",") (to_string a) *)
  | Coh _ -> "coh"
  | Var x -> x
  | Abs (i, x, a, t) ->
    if i = `Implicit then
      Printf.sprintf "fun {%s : %s} => %s" x (to_string a) (to_string t) |> pa
    else
      Printf.sprintf "fun (%s : %s) => %s" x (to_string a) (to_string t) |> pa
  | App (i, t, u) ->
    let isnt_app e = match e.desc with App _ -> false | _ -> true in
    if i = `Implicit then Printf.sprintf "%s {%s}" (to_string ~pa:(isnt_app t) t) (to_string u) |> pa
    else Printf.sprintf "%s %s" (to_string ~pa:(isnt_app t) t) (to_string ~pa:true u) |> pa
  | Pi (i, x, a, t) ->
    if i = `Implicit then
      Printf.sprintf "{%s : %s} => %s" x (to_string a) (to_string t) |> pa
    else
      Printf.sprintf "(%s : %s) => %s" x (to_string a) (to_string t) |> pa
  | Obj -> "."
  | Hom (a, b) -> Printf.sprintf "%s → %s" (to_string ~pa:true a) (to_string b) |> pa
  | Prod (a, b) -> Printf.sprintf "%s × %s" (to_string ~pa:true a) (to_string b) |> pa
  | One -> "1"
  | Id (a, t, u) ->
    Printf.sprintf "(%s = %s : %s)" (to_string ~pa:true t) (to_string ~pa:true u) (to_string ~pa:true a) |> pa
  | Meta m ->
    (
      match m.value with
      | Some t -> to_string t
      | _ -> Printf.sprintf "?%d" m.id
    )
  | Type -> "Type"

let string_of_implicit = function
  | `Implicit -> "implicit"
  | `Explicit -> "explicit"

let string_of_context env = List.map (fun (x,v) -> x ^ " = " ^ to_string v) env |> String.concat ","

(** Create an expression from its contents. *)
let mk ?pos desc : t =
  (* assert (pos <> None); *)
  let pos = Option.value ~default:Pos.dummy pos in
  { desc; pos }

let var ?pos x = mk ?pos (Var x)

let meta_counter = ref 0

let meta ?pos ?(real=false) ?value ty =
  incr meta_counter;
  mk ?pos (Meta { pos = if real then pos else None; value; id = !meta_counter; ty })

(* Real means that the hole was written in the original source (so that we should display its elaborated contents). *)
let hole ?pos ?(real=false) () =
  meta ?pos ~real (meta ?pos (mk ?pos Type))

(** Create coherence with abstracted arguments. *)
let abs_coh ?pos l a =
  let mk = mk ?pos in
  let rec aux = function
    | (i,x,a)::l -> mk (Abs (i,x, a, aux l))
    | [] ->
      let l = List.map (fun (_i,x,a) -> x,a) l in
      mk (Coh (l, a))
  in
  aux l

(** Commands a toplevel actions. *)
type command =
  | Let of string * t option * t (** declare a value *)
  | Check of t (** infer the type of an expression *)
  | NCoh of context * t (** ensure that we are *not* coherent *)

(** A program. *)
type prog = command list

let fresh_var_name =
  let n = ref 0 in
  fun () ->
    incr n;
    Printf.sprintf "x#%d" !n

let fresh_var () = fresh_var_name () |> var

(** Build multiple hom types. *)
let rec homs ?pos l t =
  match l with
  | a::l -> mk ?pos (Hom (a, homs ?pos l t))
  | [] -> t

(** Build multiple abstractions. *)
let rec abss ?pos l e =
  match l with
    | (i,x,a)::l -> mk ?pos (Abs (i, x, a, abss ?pos l e))
    | [] -> e

(** Build multiple pi types. *)
let rec pis ?pos l t =
  match l with
  | (i,x,a)::l -> mk ?pos (Pi (i, x, a, pis ?pos l t))
  | [] -> t

(* Test whether an expression has a free variable. *)
let rec has_fv x e =
  match e.desc with
  | Var y -> x = y
  | Hom (a, b) -> has_fv x a || has_fv x b
  | Prod (a, b) -> has_fv x a || has_fv x b
  | Id (a, t, u) -> has_fv x a || has_fv x t || has_fv x u
  | App (_, t, u) -> has_fv x t || has_fv x u
  | Meta { value = Some t; ty = ty; _ } -> has_fv x t || has_fv x ty
  | Meta { value = None; ty = ty; _ } -> has_fv x ty
  | Obj -> false
  | _ -> error ~pos:e.pos "has_fv: handle %s" (to_string e)

let is_implicit_pi e =
  match e.desc with
  | Pi(`Implicit,_,_,_) -> true
  | _ -> false

(** Check whether a type is a pasting scheme. *)
let check_ps_type a =
  (* printf "* check_ps %s\n%!" (to_string a); *)
  let rec target a =
    match a.desc with
    | Var x -> x
    | Hom (_, b) -> target b
    | Id _ -> failure a.pos "identities are not handled here yet"
    | _ -> assert false
  in
  let rec arguments a =
    match a.desc with
    | Var _ -> []
    | Hom (a, b) -> a::(arguments b)
    | _ -> assert false
  in
  let has_target x a =
    target a = x
  in
  (* Find the unique proof of a type. vars is the list of head variables and env is the list of known arguments. *)
  let rec prove vars env c =
    match c.desc with
    | Var x ->
      (
        match List.find_and_remove_opt (has_target x) env with
        | Some (a, env) -> List.iter (prove vars env) (arguments a)
        | None -> failure c.pos "no producer for %s" x
      )
    | Hom (a, b) ->
      let x = target a in
      if List.mem x vars then failure c.pos "multiple producers for %s" x;
      prove (x::vars) (a::env) b
    | Id _ -> assert false
    | Obj -> failure c.pos "cannot prove identities between objects"
    | _ -> assert false
  in
  prove [] [] a

(** Whether a type in a context is a pasting scheme. *)
let check_ps ?pos l a =
  (* printf "* check_ps: %s\n%!" (to_string (pis l a)); *)
  (* Remove variable declarations from the context. *)
  let vars, l =
    let split_vars l =
      let is_obj a = a.desc = Obj in
      let vars = List.filter_map (fun (x,a) -> if is_obj a then Some x else None) l in
      let l = List.filter (fun (_,a) -> not (is_obj a)) l in
      vars, l
    in
    split_vars l
  in
  (* Remove indentities in arguments. *)
  let vars, l, a =
    (* Apply rewriting rules. *)
    let rec rewrite rw e =
      let rewrite = rewrite rw in
      let mk = mk ~pos:e.pos in
      match e.desc with
      | Var x ->
        (
          match List.assoc_opt x rw with
          | Some e' -> mk e'.desc
          | None -> e
        )
      | Hom (a, b) -> mk (Hom (rewrite a, rewrite b))
      | Prod (a, b) -> mk (Prod (rewrite a, rewrite b))
      | One -> mk One
      | Id (a, t, u) -> mk (Id (rewrite a, rewrite t, rewrite u))
      | Obj -> e
      | App (i, t, u) -> mk (App (i, rewrite t, rewrite u))
      | Meta { value = Some t; _ } -> rewrite t
      | _ -> error ~pos:e.pos "TODO: in rewrite handle %s" (to_string e)
    in
    (* Orient identities on variables as rewriting rules and normalize l. *)
    let rec aux rw = function
      | (_, {desc = Id (_, {desc = Var x; _}, t); _})::l
      | (_, {desc = Id (_, t, {desc = Var x; _}); _})::l ->
        assert (not (has_fv x t));
        let t = rewrite rw t in
        let rw = List.map (fun (y,u) -> y, rewrite [x,t] u) rw in
        (* printf "rewrite: %s -> %s\n%!" x (to_string t); *)
        aux ((x,t)::rw) l
      | (_, {desc = Id _; pos})::_ -> failure pos "could not eliminate identity"
      | (x, a)::l ->
        let a = rewrite rw a in
        let rw, l = aux rw l in
        rw, (x,a)::l
      | [] -> rw, []
    in
    let rw, l = aux [] l in
    (* printf "rw: %s\n%!" (List.map (fun (x,t) -> Printf.sprintf "%s -> %s" x (to_string t)) rw |> String.concat ", "); *)
    (* Remove rewritten variables. *)
    let l = List.filter (fun (x,_) -> not (List.mem_assoc x rw)) l in
    let vars = List.diff vars (List.map fst rw) in
    vars, l, rewrite rw a
  in
  (* printf "**** after removal: %s\n%!" (to_string (pis l a)); *)
  (* Ensure that the declared variables are exactly the free variables *)
  let () =
    let a = homs ?pos (List.map snd l) a in
    let fv a =
      let rec aux a fv =
        match a.desc with
        | Var x -> if not (List.mem x fv) then x::fv else fv
        | Obj -> fv
        | Hom (a, b) -> fv |> aux a |> aux b
        | Prod (a, b) -> fv |> aux a |> aux b
        | One -> fv
        | Id (a, t, u) -> fv |> aux a |> aux t |> aux u
        | App (_, t, u) -> fv |> aux t |> aux u
        | _ -> failwith "TODO: fv handle %s" (to_string a)
      in
      aux a []
    in
    let d = List.diff vars (fv a) in
    if d <> [] then failure a.pos "unused variables: %s" (String.concat ", " d)
  in
  (* An identity is provable when its type is contractible. *)
  let a =
    match a.desc with
    | Id (a, _, _) -> a
    | _ -> a
  in
  (* Turn products into arrows. *)
  let rec deproduct e =
    match e.desc with
    | Hom (a, b) ->
      let aa = deproduct a in
      let bb = deproduct b in
      List.map (fun b -> homs ~pos:e.pos aa b) bb
    | Prod (a, b) -> (deproduct a)@(deproduct b)
    | One -> []
    | Var _
    | Obj -> [e]
    | _ -> failwith "TODO: deproduct handle %s" (to_string e)
  in
  let aa =
    let l = List.map snd l in
    let l = List.map deproduct l |> List.flatten in
    List.map (homs ?pos l) (deproduct a)
  in
  List.iter check_ps_type aa

(** All metavariables of a term. *)
let metavariables e =
  let rec aux e acc =
  match e.desc with
  | Coh (l, a) -> List.fold_left (fun acc (_, a) -> aux a acc) acc l |> aux a
  | Var _ -> acc
  | Abs (_, _, a, t) -> acc |> aux a |> aux t
  | App (_, t, u) -> acc |> aux t |> aux u
  | Obj -> acc
  | Pi (_, _, a, b)
  | Hom (a, b)
  | Prod (a, b) -> acc |> aux a |> aux b
  | One -> acc
  | Id (a, t, u) -> acc |> aux a |> aux t |> aux u
  | Meta m ->
    (
      let acc = if List.memq m acc then acc else m::acc in
      match m.value with
      | Some v -> acc |> aux v |> aux m.ty
      | None -> acc |> aux m.ty
    )
  | Type -> acc
  in
  aux e []

let has_metavariable m e = List.mem m (metavariables e)

exception Unification

(** Make sure that two values are equal (and raise [Unification] if this cannot be the case). *)
(* The first argument is the alpha-conversion to apply to t *)
let rec unify tenv env ?(alpha=[]) t t' =
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
      | Some v -> v
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
and infer tenv env e =
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
    check_ps ~pos l a;
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
  | Abs(`Implicit,x,a,t), Pi(`Implicit,x',a',b) when x = x' (* TODO: alpha? *)->
    unify tenv env a a';
    let t = check ((x,a)::tenv) ((x,var x)::env) t b in
    mk ~pos:e.pos (Abs(`Implicit,x,a,t))
  | _ ->
    let e, b = infer tenv env e in
    try
      if not (b.desc = Obj && a.desc = Type) then unify tenv env b a; e
    with
    | Unification ->
      if is_implicit_pi b && not (is_implicit_pi a) then
        let e = mk ~pos:e.pos (App (`Implicit, e, hole ~pos:e.pos ())) in
        check tenv env e a
      else failure e.pos "got %s but %s expected" (to_string b) (to_string a)

let print_metavariables_elaboration m =
  List.iter
    (fun (m:meta) ->
       if m.pos <> None then
         let v =
           match m.value with
           | Some v -> to_string v
           | None -> "?"
         in
         printf "=?.?= at %s, ?%d elaborated to %s\n" (Pos.to_string (Option.get m.pos)) m.id v
    ) (List.sort compare m)

let print_unelaborated_metavariables m =
  List.iter
    (fun (m:meta) ->
       if m.value = None then
         printf "=?.?= warning: unelaborated ?%d at %s\n%!" m.id (Pos.Option.to_string m.pos)
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
    printf "=^.^= defined %s : %s\n%!" x (to_string a);
    (* printf "      %s\n%!" (to_string v); *)
    tenv, env
  | Check e ->
    let e, a = infer tenv env e in
    printf "=^.^= check %s : %s\n%!" (Pos.to_string e.pos) (to_string a);
    tenv, env
  | NCoh (l, a) ->
    check tenv env (pis ~pos:a.pos (List.map (fun (x,a) -> `Explicit,x,a) l) a) (mk ~pos:a.pos Type) |> ignore;
    (try check_ps ~pos:a.pos l a; failure a.pos "expression accepted as a coherence" with _ -> ());
    tenv, env

(** Execute a program. *)
let exec = List.fold_left exec_command
