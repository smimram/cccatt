(** Check for pasting schemes. *)

open Extlib
open Common
open Term

(** Check whether a type is a pasting scheme in the theory of ccc, for an implicational formula. *)
let check_ccc a =
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
  let () =
    let rec depth a =
      match a.desc with
      | Var _ -> 0
      | Hom (a, b) -> max (1 + depth a) (depth b)
      | _ -> assert false
    in
    match !Setting.depth with
    | Some d -> if depth a > d then failure a.pos "pasting has depth %d but we are limited to %d" (depth a) d
    | None -> ()
  in
  prove [] [] a

(** Whether a type in a context is a pasting scheme. *)
let check ~pos l a =
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
      | Meta { value = None; _ } -> error ~pos:e.pos "unresolved metvariable %s when checking pasting conditions" (to_string e)
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
    let a = homs ~pos (List.map snd l) a in
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
      List.map (homs ~pos l) (deproduct a)
    in
    List.iter check_ccc aa

  | `Plain ->
    let l = List.map snd l in
    assert (List.mem a l)

(*
  | `Monoid ->
    let rec get_prod a =
      match (unmeta a).desc with
      | Var _ -> [a]
      | Prod (a,b) -> (get_prod a)@(get_prod b)
      | One -> []
      | _ -> failure a.pos "product expected"
    in
    let a = get_prod a in
    let l = l |> List.map snd |> List.map get_prod in
*)

  | `Category ->
    let get_arr a =
      match (unmeta a).desc with
      | Hom (a, b) ->
        if not (is_var a) then failure a.pos "variable expected";
        if not (is_var b) then failure b.pos "variable expected";
        a, b
      | _ ->
        failure a.pos "arrow expected"
    in
    let l = List.map snd l in
    let l = List.map get_arr l in
    let a, b = get_arr a in
    let eq = eq_var in
    (* Check that we have a unique path from b to a. *)
    let rec check seen b =
      (* Find the unique antecedent. *)
      let rec find = function
        | (a,b')::l when eq b b' ->
          List.iter (fun (_,b') -> if eq b b' then failure b'.pos "multiple producers for %s" (to_string b)) l;
          Some a
        | _::l -> find l
        | [] -> None
      in
      let seen = b::seen in
      match find l with
      | Some a ->
        if List.exists (fun a' -> eq a a') seen then failure pos "cyclic dependencies";
        check seen a
      | None ->
        (* Note that we ensure that a is terminal so that there is no cycle (ie another path from a to b) *)
        if not (eq a b) then failure b.pos "no producer for %s" (to_string b)
    in
    check [] b

  | `Monoidal ->
    (* Make sure that the types are arrows between tensor expressions. *)
    let rec get_tens a =
      match (unmeta a).desc with
      | Prod (a, b) -> (get_tens a)@(get_tens b)
      | One -> []
      | Var _ -> [a]
      | _ -> failure a.pos "tensor product of variables expected"
    in
    let get_arr a =
      match (unmeta a).desc with
      | Hom (a, b) ->
        let a' = get_tens a in
        let b' = get_tens b in
        if a' = [] then failure a.pos "type cannot be empty";
        if b' = [] then failure b.pos "type cannot be empty";
        a', b'
      | _ -> failure a.pos "arrow expected"
    in
    let l = List.map snd l in
    let l = List.map get_arr l in
    let a, b = get_arr a in
    let eq = eq_var in
    (* NOTE: we could also check that the formula is balanced *)
    (* Make sure that sources and targets have distinct variables and are disjoint. *)
    List.iter (fun (a,b) ->
        let distinct a = List.iter_unordered_pairs (fun x y -> if eq x y then failure y.pos "repeated variable") a in
        distinct a;
        distinct b;
        (* NOTE: should already be handled by seen below *)
        (* List.iter (fun x -> List.iter (fun y -> if eq x y then failure y.pos "looping variable") b) a *)
      ) l;
    (* Make sure that sources and targets are pairwise disjoint. *)
    List.iter_unordered_pairs
      (fun (a,b) (a',b') ->
         let disjoint st a b = List.iter (fun x -> List.iter (fun y -> if eq x y then failure y.pos "variable already used in %s: %s" st (to_string y)) b) a in
         disjoint "source" a a';
         disjoint "target" b b'
      ) l;
    (* Make sure that we have a 2-path from b to a. *)
    let rec check seen b =
      (* Printf.printf "check : %s\n%!" (to_string (prods b)); *)
      (* What remains of a after b *)
      let rec residual a b =
        match a, b with
        | x::a, y::b -> if eq x y then residual a b else None
        | _, [] -> Some a
        | [], _ -> None
      in
      (* Find a possible rewrite of b' to a' in b. *)
      let rewrite (a',b') =
        let rec aux pre b =
          match residual b b' with
          | Some c ->
            (* Printf.printf "%s (%s -> %s) %s\n%!" (to_string (prods (List.rev pre))) (to_string (prods a')) (to_string (prods b)) (to_string (prods c)); *)
            Some (List.rev pre, a', c)
          | None ->
            match b with
            | x::b -> aux (x::pre) b
            | [] -> None
        in
        aux [] b
      in
      let rw = List.find_map rewrite l in
      match rw with
      | Some (u, a, c) ->
        List.iter (fun x -> if List.exists (eq x) seen then failure pos "cyclic dependency on %s" (to_string x)) a;
        check (a@seen) (u@a@c)
      | None -> if not (List.length a = List.length b && List.for_all2 eq a b) then failure pos "no producer for %s" (to_string (prods b))
    in
    check [] b

  | `Cartesian ->
    let module S = Set.Make(struct type nonrec t = t let compare = compare_var end) in
    let rec get_prod ?(distinct=false) a =
      let a0 = a in
      match (unmeta a).desc with
      | Prod (a, b) ->
        let a = get_prod a in
        let b = get_prod b in
        if distinct && not (S.disjoint a b) then failure a0.pos "repeated variables";
        S.union a b
      | One -> S.empty
      | Var _ -> S.singleton a
      | _ -> failure a.pos "product of variables expected"
    in
    let get_arr a =
      match (unmeta a).desc with
      | Hom (a, b) -> (a, b)
      | _ -> failure a.pos "arrow expected"
    in
    let l = List.map snd l in
    let l = List.map get_arr l in
    let l = List.map (fun (a,b) -> get_prod a, get_prod ~distinct:true b) l in
    let a, b = get_arr a in
    let a = get_prod ~distinct:true a in
    let b = get_prod b in
    (* From the source to the targets. *)
    let rec check available l =
      if l = [] then available else
        match List.find_and_remove_opt (fun (a,_) -> S.subset a available) l with
        | Some ((_,b),l) ->
          if not (S.disjoint b available) then failure pos "variable produced multiple times: %s" (S.to_seq b |> List.of_seq |> List.map to_string |> String.concat ", ");
          check (S.union b available) l
        | None -> failure pos "some hypothesis could not be produced"
    in
    let available = check a l in
    if not (S.subset b available) then failure pos "some variables cannot be produced: %s" (S.diff b available |> S.to_seq |> List.of_seq |> List.map to_string |> String.concat ", ")

  | `Symmetric_monoidal ->
    let module S = Set.Make(struct type nonrec t = t let compare = compare_var end) in
    (* Make sure that the types are arrows between tensor expressions. *)
    let rec get_tens a =
      let pos = a.pos in
      match (unmeta a).desc with
      | Prod (a, b) ->
        let a = get_tens a in
        let b = get_tens b in
        if not (S.disjoint a b) then failure pos "repeated variables";
        S.union a b
      | One -> S.empty
      | Var _ -> S.singleton a
      | _ -> failure a.pos "tensor product of variables expected"
    in
    let get_arr a =
      match (unmeta a).desc with
      | Hom (a, b) ->
        let a' = get_tens a in
        let b' = get_tens b in
        if S.is_empty a' then failure a.pos "type cannot be empty";
        if S.is_empty b' then failure b.pos "type cannot be empty";
        (* if not (S.disjoint a' b') then failure pos "looping type"; *)
        a', b'
      | _ -> failure a.pos "arrow expected"
    in
    let l = List.map snd l in
    let l = List.map get_arr l in
    let a, b = get_arr a in
    (* Make sure that sources and targets are pairwise disjoint. *)
    (* NOTE: we could also check that the formula is balanced *)
    List.iter_unordered_pairs
      (fun (a,b) (a',b') ->
         if not (S.disjoint a a') then failure pos "sources not disjoint";
         if not (S.disjoint b b') then failure pos "targets not disjoint"
      ) l;
    (* Make sure that we have a 2-path from b to a. *)
    let rec check seen b =
      (* Printf.printf "check : %s\n%!" (to_string (prods b)); *)
      let rule = List.find_opt (fun (_,b') -> S.subset b' b) l in
      match rule with
      | Some (a',b') ->
        if not (S.disjoint a' seen) then failure pos "cyclic dependency";
        check (S.union a' seen) (S.union (S.diff b b') a')
      | None ->
        if not (S.equal a b) then failure pos "no producer for %s" (to_string (prods (S.elements b)))
    in
    check S.empty b

  | `Symmetric_monoidal_closed ->
    let module S = Set.Make(struct type nonrec t = t let compare = compare_var end) in
    let a = homs ~pos (List.map snd l) a in
    (* Check that the formula is balanced (every variable occurs at most once positively and at most once negatively. *)
    let rec balanced neg pos a =
      match a.desc with
      | Var _ -> if S.mem a pos then failure a.pos "variable occurs twice with the same polarity"
      | One -> ()
      | Prod (a, b) -> balanced neg pos a; balanced neg pos b
      | Hom (a, b) -> balanced pos neg a; balanced neg pos b
      | _ -> assert false
    in
    balanced S.empty S.empty a;
    let eq = eq_var in
    (* Prove a. *) 
    let rec prove env a =
      (* Printf.printf "prove: %s\n%!" (to_string a); *)
      match a.desc with
      | Var _ ->
        if not (List.exists (eq a) env) then failure a.pos "cannot produce %s" (to_string a);
        List.filter (fun b -> not (eq a b)) env
      | Prod (a, b) -> prove (prove env a) b
      | Hom (a, b) -> coprove a env b
      | _ -> failwith "TODO: smcc pasting, handle %s" (to_string a)
    (* Add a to the environment, simplifying it. *)
    and coprove a env b =
      match a.desc with
      | Var _ -> prove (a::env) b
      | Hom (a, a') -> coprove a' (prove env a) b
      | _ -> failwith "TODO: smcc pasting (coprove), handle %s" (to_string a)
    in
    let env = prove [] a in
    if env <> [] then failure pos "unused hypothesis: %s" (env |> List.map to_string |> String.concat ", ")
      
  | _ -> failwith "unhandled mode"
