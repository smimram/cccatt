open Extlib

module V = Value

let failure pos fmt =
  Printf.ksprintf (fun s -> failwith (Pos.to_string pos ^ ": " ^ s)) fmt

(** An expression. *)
type t =
  {
    desc : desc;
    pos : Pos.t;
  }

and desc =
  | Coh of context * t
  | Var of string
  | Abs of string * t * t
  | App of t * t
  | Pi of string * t * t
  | Obj
  | Hom of t * t
  | Id of (t option ref * t * t)
  | Type

and context = (string * t) list

(** String representation of an expression. This should mostly be useful for debugging (we want to print values). *)
let rec to_string e =
  match e.desc with
  | Coh _ -> "coh"
  | Var x -> x
  | Abs (x, a, t) -> Printf.sprintf "fun (%s : %s) => %s" x (to_string a) (to_string t)
  | App (t, u) -> Printf.sprintf "%s %s" (to_string t) (to_string u)
  | Pi (x, a, t) -> Printf.sprintf "(%s : %s) => %s" x (to_string a) (to_string t)
  | Obj -> "*"
  | Hom (t, u) -> Printf.sprintf "(%s -> %s)" (to_string t) (to_string u)
  | Id (a, t, u) ->
    let a =
      match !a with
      | Some a -> "[" ^ to_string a ^ "]"
      | None -> ""
    in
    Printf.sprintf "(%s =%s %s)" (to_string t) a (to_string u)
  | Type -> "Type"

let string_of_context env = List.map (fun (x,v) -> x ^ " = " ^ V.to_string v) env |> String.concat ","

(** Create an expression from its contents. *)
let mk ?pos desc : t =
  let pos = Option.value ~default:Pos.dummy pos in
  { desc; pos }

type command =
  | Let of string * t (** declare a value *)
  | Check of t (** infer the type of an expression *)
  | NCoh of context * t (** ensure that we are *not* coherent *)

(** A program. *)
type prog = command list

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

(** Check whether a type is a pasting scheme. *)
let check_ps a =
  (* Printf.printf "check_ps %s\n%!" (to_string a); *)
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
  Printf.printf "**** check_ps: %s\n%!" (to_string (pis l a));
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
      | Id (a, t, u) -> mk (Id (Option.map rewrite !a |> ref, rewrite t, rewrite u))
      | Obj -> e
      | _ -> failwith (Printf.sprintf "TODO: handle %s" (to_string e))
    in
    (* Test whether an expression has a free variable. *)
    let rec has_fv x e =
      match e.desc with
      | Var y -> x = y
      | Hom (a, b) -> has_fv x a || has_fv x b
      | Id (a, t, u) -> (Option.map (has_fv x) !a |> Option.value ~default:false) || has_fv x t || has_fv x u
      | Obj -> false
      | _ -> assert false
    in
    (* Orient identities on variables as rewriting rules and normalize l. *)
    let rec aux rw = function
      | (_, {desc = Id (_, {desc = Var x; _}, t); _})::l
      | (_, {desc = Id (_, t, {desc = Var x; _}); _})::l ->
        assert (not (has_fv x t));
        let t = rewrite rw t in
        let rw = List.map (fun (y,u) -> y, rewrite [x,t] u) rw in
        Printf.printf "rewrite: %s -> %s\n%!" x (to_string t);
        aux ((x,t)::rw) l
      | (_, {desc = Id _; pos})::_ -> failure pos "could not eliminate identity"
      | (x, a)::l ->
        let a = rewrite rw a in
        let rw, l = aux rw l in
        rw, (x,a)::l
      | [] -> rw, []
    in
    let rw, l = aux [] l in
    Printf.printf "rw: %s\n%!" (List.map (fun (x,t) -> Printf.sprintf "%s -> %s" x (to_string t)) rw |> String.concat ", ");
    (* Remove rewritten variables. *)
    let l = List.filter (fun (x,_) -> not (List.mem_assoc x rw)) l in
    let vars = List.diff vars (List.map fst rw) in
    vars, l, rewrite rw a
  in
  Printf.printf "**** after removal: %s\n%!" (to_string (pis l a));
  (* Ensure that the declared variables are exactly the free variables *)
  let () =
    let a = homs (List.map snd l) a in
    let fv a =
      let rec aux a fv =
        match a.desc with
        | Var x -> if not (List.mem x fv) then x::fv else fv
        | Obj -> fv
        | Hom (a, b) -> fv |> aux a |> aux b
        | Id (a, t, u) -> fv |> aux (Option.get !a) |> aux t |> aux u
        | _ -> assert false
      in
      aux a []
    in
    let d = List.diff vars (fv a) in
    if d <> [] then failure a.pos "unused variables: %s" (String.concat ", " d)
  in
  (* An identity is provable when its type is contractible. *)
  let a =
    match a.desc with
    | Id (a, _, _) -> Option.get !a
    | _ -> a
  in
  let a = homs (List.map snd l) a in
  check_ps a

(** Evaluate an expression to a value. *)
let rec eval env e =
  match e.desc with
  | Coh (l,_) ->
    let rec aux t = function
      | _::l -> V.Abs (fun v -> aux (V.App (t, v)) l)
      | [] -> V.Neutral t
    in
    aux V.Coh l
  | Var x ->
    (
      match List.assoc_opt x env with
      | Some v -> v
      | None -> failure e.pos "unexpected error: value for %s not found" x
    )
  | Abs (x,_,e) -> V.Abs (fun v -> eval ((x,v)::env) e)
  | App (t,u) ->
    (
      match eval env t with
      | Abs f -> f (eval env u)
      | _ -> assert false
    )
  | Pi (x, a, b) ->
    let a = eval env a in
    let b v = eval ((x,v)::env) b in
    V.Pi (a, b)
  | Id (_, t, u) ->
    let t = eval env t in
    let u = eval env u in
    V.Id (t, u)
  | Obj -> V.Obj
  | Hom (a, b) -> V.Hom (eval env a, eval env b)
  | Type -> V.Type

(** Infer the type of an expression. *)
let rec infer k tenv env e =
  (* Printf.printf "* infer %s\n%!" (to_string e); *)
  (* Printf.printf "  env : %s\n%!" (string_of_context env); *)
  match e.desc with
  | Coh (l, a) ->
    check k tenv env (pis l a) V.Type;
    check_ps l a;
    let rec aux env = function
      | (x,a)::l ->
        let a = eval env a in
        V.Pi (a, fun v -> aux ((x,v)::env) l)
      | [] -> eval env a
    in
    aux env l
  | Var x ->
    (
      match List.assoc_opt x tenv with
      | Some a -> a
      | None -> failure e.pos "unknown variable %s" x
    )
  | Abs (x,a,t) ->
    check k tenv env a V.Type;
    let a = eval env a in
    let _ =
      let x' = V.var k in
      infer (k+1) ((x, a)::tenv) ((x, x')::env) t
    in
    let b v = infer k ((x,a)::tenv) ((x,v)::env) t in
    V.Pi (a, b)
  | App (t, u) ->
    (
      match infer k tenv env t with
      | Pi (a, b) ->
        check k tenv env u a;
        let v = eval env u in
        b v
      | a -> failure t.pos "of type %s but a function was expected" (V.to_string a)
    )
  | Pi (x, a, b) ->
    check k tenv env a V.Type;
    check (k+1) ((x, eval env a)::tenv) ((x, V.var k)::env) b V.Type;
    V.Type
  | Id (a, t, u) ->
    let a : V.t =
      match !a with
      | Some a -> eval env a
      | None ->
        let v = infer k tenv env t in
        let a' =
          let mk = mk ~pos:e.pos in
          let rec readback = function
            | V.Obj -> mk Obj
            | V.Neutral (V.Var _) as var -> mk (Var (List.assoc' var env))
            | V.Hom (a, b) -> mk (Hom (readback a, readback b))
            | v -> failwith ("unhandled readback: " ^ V.to_string v)
          in
          readback v
        in
        (* Printf.printf "infered %s : %s\n%!" (to_string e) (to_string a'); *)
        (* Printf.printf "env: %s\n%!" (string_of_context env); *)
        a := Some a';
        v
    in
    (* check k tenv env t V.Obj; *)
    check k tenv env t a;
    check k tenv env u a;
    V.Type
  | Obj -> V.Type
  | Hom (a, b) ->
    check k tenv env a V.Obj;
    check k tenv env b V.Obj;
    V.Obj
  | Type -> V.Type

and check k tenv env e a =
  let b = infer k tenv env e in
  if not (V.eq k b a || (b = V.Obj && a = V.Type)) then failure e.pos "got %s but %s expected" (V.to_string b) (V.to_string a)

(** Execute a program. *)
let rec exec tenv env p =
  match p with
  | (Let (x, e))::p ->
    (* Printf.printf "*** let %s\n%!" x; *)
    (* print_endline "inferring"; *)
    let a = infer 0 tenv env e in
    (* print_endline "checking"; *)
    let v = eval env e in
    let tenv = (x,a)::tenv in
    let env = (x,v)::env in
    Printf.printf "* defined %s : %s\n%!" x (V.to_string a);
    exec tenv env p
  | (Check e)::p ->
    let a = infer 0 tenv env e in
    Printf.printf "* check %s : %s\n%!" (Pos.to_string e.pos) (V.to_string a);
    exec tenv env p
  | (NCoh (l, a))::p ->
    check 0 tenv env (pis l a) V.Type;
    (try check_ps l a; failure a.pos "expression accepted as a coherence" with _ -> ());
    exec tenv env p
  | [] -> ()
