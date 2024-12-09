open Extlib
open Common
open Term

(** Check whether a type is a pasting scheme. *)
let check_type a =
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
let check ?pos l a =
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
    let rec rewrite rw (e:Term.t) =
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
  (* Ensure that the declared variables are exactly the free variables. *)
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
  match !Setting.mode with
  | `Cartesian_closed ->
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
    List.iter check_type aa
  | `Monoidal ->
    (
      match a.desc with
      | Hom (a,b) ->
        let rec flatten a =
          match a.desc with
          | Prod (a,b) -> (flatten a)@(flatten b)
          | One -> []
          | Var _ -> [a]
          | _ -> failwith "unexpected type: %s" (to_string a)
        in
        let a = flatten a in
        let b = flatten b in
        let rec unique vars = function
          | (x:t)::l ->
            if List.mem x vars then failure x.pos "duplicated variable: %s" (to_string x);
            unique (x::vars) l
          | [] -> ()
        in
        let unique = unique [] in
        unique a;
        unique b;
        if not (a = b) then failwith "not the same variables"
      | _ -> failure a.pos "arrow expected"
    )
  | _ -> assert false
