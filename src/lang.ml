(** The CCaTT language. *)

open Extlib
open Common

let failure pos fmt =
  Printf.ksprintf (fun s -> failwith (Pos.to_string pos ^ ": " ^ s)) fmt

(** An expression. *)
type t =
  {
    desc : desc;
    pos : Pos.t;
  }

and desc =
  | Coh of context * t (** coherence *)
  | Var of string (** variable *)
  | Abs of string * t * t (** abstraction *)
  | App of t * t (** application *)
  | Pi of string * t * t (** Π-type *)
  | Obj (** object type *)
  | Hom of t * t (** hom type *)
  | Prod of t * t (** product type *)
  | Id of t * t * t (** identity type *)
  | Hole of t * t (** hole along with its type *)
  | Meta of meta (** a variable to be unified *)
  | Type (** the type of types *)

and context = (string * t) list

and meta =
  {
    pos : Pos.t option;
    id : int;
    mutable value : t option;
  }

(** String representation of an expression. This should mostly be useful for debugging (we want to print values). *)
let rec to_string ?(pa=false) e =
  let pa s = if pa then "(" ^ s ^ ")" else s in
  match e.desc with
  (* | Coh (l, a) -> Printf.sprintf "coh[%s|%s]" (List.map (fun (x,a) -> Printf.sprintf "%s:%s" x (to_string a)) l |> String.concat ",") (to_string a) *)
  | Coh _ -> "coh"
  | Var x -> x
  | Abs (x, a, t) -> Printf.sprintf "fun (%s : %s) => %s" x (to_string a) (to_string t)
  | App (t, u) -> Printf.sprintf "%s %s" (to_string t) (to_string ~pa:true u)
  | Pi (x, a, t) -> Printf.sprintf "(%s : %s) => %s" x (to_string a) (to_string t)
  | Obj -> "."
  | Hom (a, b) -> Printf.sprintf "%s → %s" (to_string ~pa:true a) (to_string b) |> pa
  | Prod (a, b) -> Printf.sprintf "%s × %s" (to_string ~pa:true a) (to_string b) |> pa
  | Id (a, t, u) ->
    Printf.sprintf "(%s = %s : %s)" (to_string ~pa:true t) (to_string ~pa:true u) (to_string ~pa:true a) |> pa
  | Hole (t, _) -> Printf.sprintf "[%s]" (to_string t)
  | Meta m ->
    (
      match m.value with
      | Some t -> to_string t
      | _ -> Printf.sprintf "?%d" m.id
    )
  | Type -> "Type"

let string_of_context env = List.map (fun (x,v) -> x ^ " = " ^ to_string v) env |> String.concat ","

(** Create an expression from its contents. *)
let mk ?pos desc : t =
  let pos = Option.value ~default:Pos.dummy pos in
  { desc; pos }

let var ?pos x = mk ?pos (Var x)

let meta_counter = ref 0

let hole ?pos () =
  let t = mk ?pos (Meta { pos = pos; value = None; id = !meta_counter }) in
  incr meta_counter;
  let a = mk (Meta { pos = None; value = None; id = !meta_counter }) in
  incr meta_counter;
  mk ?pos (Hole (t, a))

(** Create coherence with abstracted arguments. *)
let abs_coh ?pos l a =
  let mk = mk ?pos in
  let rec aux = function
    | (x,a)::l -> mk (Abs (x, a, aux l))
    | [] -> mk (Coh (l, a))
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

(** Build multiple hom types. *)
let rec homs ?pos l t =
  match l with
  | a::l -> mk ?pos (Hom (a, homs ?pos l t))
  | [] -> t

(** Build multiple pi types. *)
let rec pis ?pos l t =
  match l with
  | (x,a)::l -> mk ?pos (Pi (x, a, pis ?pos l t))
  | [] -> t

(* Test whether an expression has a free variable. *)
let rec has_fv x e =
  match e.desc with
  | Var y -> x = y
  | Hom (a, b) -> has_fv x a || has_fv x b
  | Prod (a, b) -> has_fv x a || has_fv x b
  | Id (a, t, u) -> has_fv x a || has_fv x t || has_fv x u
  | App (t, u) -> has_fv x t || has_fv x u
  | Hole (t, a) -> has_fv x t || has_fv x a
  | Meta { value = Some t; _ } -> has_fv x t
  | Meta { value = None; _ } -> true
  | Obj -> false
  | _ -> error ~pos:e.pos "has_fv: handle %s" (to_string e)


(** Check whether a type is a pasting scheme. *)
let check_ps a =
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
let check_ps l a =
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
      | Id (a, t, u) -> mk (Id (rewrite a, rewrite t, rewrite u))
      | Obj -> e
      | App (t, u) -> mk (App (rewrite t, rewrite u))
      | Hole (t, _) -> rewrite t
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
    let a = homs (List.map snd l) a in
    let fv a =
      let rec aux a fv =
        match a.desc with
        | Var x -> if not (List.mem x fv) then x::fv else fv
        | Obj -> fv
        | Hom (a, b) -> fv |> aux a |> aux b
        | Prod (a, b) -> fv |> aux a |> aux b
        | Id (a, t, u) -> fv |> aux a |> aux t |> aux u
        | App (t, u) -> fv |> aux t |> aux u
        | _ -> failwith (Printf.sprintf "TODO: fv handle %s" (to_string a))
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
    | Prod (a, b) ->
      (deproduct a)@(deproduct b)
    | Var _
    | Obj -> [e]
    | _ -> failwith (Printf.sprintf "TODO: deproduct handle %s" (to_string e))
  in
  let aa =
    let l = List.map snd l in
    let l = List.map deproduct l |> List.flatten in
    List.map (homs l) (deproduct a)
  in
  List.iter check_ps aa

(** Replace variable x by v in e. *)
let rec subst x v e =
  let s = subst x v in
  let mk ?(pos=e.pos) = mk ~pos in
  let var ?(pos=e.pos) = var ~pos in
  match e.desc with
  | Coh (l, a) -> mk (Coh (List.map (fun (x,a) -> x, s a) l, s a))
  | Var y -> if y = x then mk v.desc else e
  | Abs (y, a, t) ->
    let y, t =
      if has_fv y v then
        let y' = fresh_var_name () in
        y', subst y (var y') t
      else y, t
    in
    mk (Abs (y, s a, s t))
  | Pi (y, a, b) ->
    let y, b =
      if has_fv y v then
        let y' = fresh_var_name () in
        y', subst y (var y') b
      else y, b
    in
    mk (Pi (y, s a, s b))
  | App (t, u) -> mk (App (s t, s u))
  | Obj -> e
  | Hom (a, b) -> mk (Hom (s a, s b))
  | Prod (a, b) -> mk (Prod (s a, s b))
  | Id (a, t, u) -> mk (Id (s a, s t, s u))
  | Hole (t, a) -> mk (Hole (s t, s a))
  | Meta { value = Some t; _ } -> s t
  | Meta { value = None; _ } -> e
  | Type -> e

(*
let subst x v e =
  let e' = subst x v e in
  Printf.printf "%s[%s/%s] = %s\n" (to_string e) (to_string v) x (to_string e');
  e'
*)
   
exception Unification

(** Make sure that two values are equal (and raise [Unification] if this cannot be the case). *)
(* The first argument is the alpha-conversion to apply to t' *)
let rec unify ?(alpha=[]) t t' =
  let unify ?(alpha=alpha) = unify ~alpha in
  match t.desc, t'.desc with
  | Var x, Var y ->
    let y = match List.assoc_opt y alpha with Some y -> y | None -> y in
    if x <> y then raise Unification
  | Hom (a, b), Hom (a', b') -> unify a a'; unify b b'
  | Obj, Obj -> ()
  | Type, Type -> ()
  | Prod (a, b), Prod (a', b') -> unify a a'; unify b b'
  | Pi (x, a, b), Pi (x', a', b') -> unify a a'; unify ~alpha:((x',x)::alpha) b b'
  | Hole (t, _), _ -> unify t t'
  | _, Hole (t', _) -> unify t t'
  | Meta { value = Some t; _ }, _ -> unify t t'
  | _, Meta { value = Some t'; _ } -> unify t t'
  | Meta m, _ ->
    (* TODO: check for cycles *)
    m.value <- Some t'
  | _, Meta m' ->
    (* TODO: check for cycles *)
    m'.value <- Some t
  | _ -> raise Unification

(** Evaluate an expression to a value. *)
let rec eval env e =
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
  | Abs (x,a,e) ->
    let x' = if List.mem_assoc x env then fresh_var_name () else x in
    mk (Abs (x', eval env a, eval ((x,var x')::env) e))
  | App (t,u) ->
    (
      match (eval env t).desc with
      | Abs (x,_,t) -> subst x u t
      | _ -> assert false
    )
  | Pi (x, a, b) ->
    let x' = if List.mem_assoc x env then fresh_var_name () else x in
    mk (Pi (x', eval env a, eval ((x,var x')::env) b))
  | Id (a, t, u) -> mk (Id (eval env a, eval env t, eval env u))
  | Obj -> mk Obj
  | Hom (a, b) -> mk (Hom (eval env a, eval env b))
  | Prod (a, b) -> mk (Prod (eval env a, eval env b))
  | Type -> mk Type
  | Hole (t, _) -> t
  | Meta { value = Some t; _ } -> eval env t
  | Meta { value = None; _ } -> e

(* Note: in the following environments only contain values, and type inference produces values. *)

(** Infer the type of an expression. *)
let rec infer tenv env e =
  (* printf "* infer %s\n%!" (to_string e); *)
  (* printf "  tenv : %s\n%!" (string_of_context tenv); *)
  (* printf "  env : %s\n%!" (string_of_context env); *)
  (* printf "\n"; *)
  match e.desc with
  | Coh (l, a) ->
    check tenv env (pis l a) (mk Type);
    check_ps l a;
    eval env a
  | Var x ->
    (
      match List.assoc_opt x tenv with
      | Some a -> a
      | None -> failure e.pos "unknown variable %s" x
    )
  | Abs (x,a,t) ->
    check tenv env a (mk Type);
    let a = eval env a in
    let b = infer ((x,a)::tenv) ((x, mk (Var x))::env) t in
    mk (Pi (x, a, b))
  | App (t, u) ->
    (
      let a = infer tenv env t in
      match a.desc with
      | Pi (x, a, b) ->
        check tenv env u a;
        let u = eval env u in
        eval ((x,u)::env) b
      | _ -> failure t.pos "of type %s but a function was expected" (to_string a)
    )
  | Pi (x, a, b) ->
    check tenv env a (mk Type);
    let a = eval env a in
    check ((x, eval env a)::tenv) ((x, var x)::env) b (mk Type);
    mk Type
  | Id (a, t, u) ->
    check tenv env a (mk Obj);
    let a = eval env a in
    check tenv env t a;
    check tenv env u a;
    mk Type
  | Obj -> (mk Type)
  | Hom (a, b) ->
    check tenv env a (mk Obj);
    check tenv env b (mk Obj);
    mk Obj
  | Prod (a, b) ->
    check tenv env a (mk Obj);
    check tenv env b (mk Obj);
    mk Obj
  | Type -> mk Type
  | Hole (_, a) -> a
  | Meta _ -> assert false

and check tenv env e a =
  (* printf "* check %s : %s\n%!" (to_string e) (V.to_string a); *)
  let b = infer tenv env e in
  try
    if not (b.desc = Obj && a.desc = Type) then unify b a
  with
  | Unification -> failure e.pos "got %s but %s expected" (to_string b) (to_string a)

(** All metavariables of a term. *)
let metavariables e =
  let rec aux e acc =
  match e.desc with
  | Coh (l, a) -> List.fold_left (fun acc (_, a) -> aux a acc) acc l |> aux a
  | Var _ -> acc
  | Abs (_, a, t) -> acc |> aux a |> aux t
  | App (t, u) -> acc |> aux t |> aux u
  | Obj -> acc
  | Pi (_, a, b)
  | Hom (a, b)
  | Prod (a, b) -> acc |> aux a |> aux b
  | Id (a, t, u) -> acc |> aux a |> aux t |> aux u
  | Hole (t, a) -> acc |> aux t |> aux a
  | Meta m -> if List.memq m acc then acc else m::acc
  | Type -> acc
  in
  aux e []

let print_metavariables_elaboration e =
  List.iter
    (fun (m:meta) ->
       if m.pos <> None then
         let v =
           match m.value with
           | Some v -> to_string v
           | None -> "?"
         in
         printf "... at %s, ?%d elaborated to %s\n" (Pos.to_string (Option.get m.pos)) m.id v
    ) (metavariables e)

let exec_command (tenv, env) p =
  match p with
  | Let (x, a, e) ->
    (* printf "*** let %s := %s\n%!" x (to_string e); *)
    (* print_endline "inferring"; *)
    let a =
      match a with
      | Some a ->
        let a = eval env a in
        print_metavariables_elaboration a;
        check tenv env e a;
        a
      | None ->
        infer tenv env e
    in
    (* print_endline "checking"; *)
    let v = eval env e in
    print_metavariables_elaboration v;
    let tenv = (x,a)::tenv in
    let env = (x,v)::env in
    printf "=^.^= defined %s : %s\n%!" x (to_string a);
    tenv, env
  | Check e ->
    let a = infer tenv env e in
    printf "=^.^= check %s : %s\n%!" (Pos.to_string e.pos) (to_string a);
    tenv, env
  | NCoh (l, a) ->
    check tenv env (pis l a) (mk Type);
    (try check_ps l a; failure a.pos "expression accepted as a coherence" with _ -> ());
    tenv, env

(** Execute a program. *)
let exec = List.fold_left exec_command
