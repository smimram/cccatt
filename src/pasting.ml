(** Check for pasting schemes. *)

open Extlib
open Common
open Term

(** A set of variables. *)
module VarSet = Set.Make(struct type nonrec t = t let compare = compare_var end)
module VS = VarSet
module StringSet = Set.Make(String)
module SS = StringSet

(* TODO: directly a set instead of constructing the list *)
let fv a = SS.of_list @@ free_variable_names a

(** Check whether a 1-dimensional type in a context is a pasting scheme. *)
let check1 ~pos l a =
  (* Printf.printf "check1 : %s ⊢ %s\n%!" (string_of_context l) (to_string a); *)
  match !Settings.mode with

  | `Plain ->

    let l = List.map snd l in
    if not (List.mem a l) then failure pos "cannot produce %s" (to_string a)

  | `Category ->

    let get_arr a =
      match (unmeta a).desc with
      | Arr (a, t, u) ->
        if not (is_obj a) then failure a.pos "1-dimensional arrow expected, got %s" (to_string a);
        if not (is_var t) then failure t.pos "variable expected";
        if not (is_var u) then failure u.pos "variable expected";
        t, u
      | _ ->
        failure a.pos "arrow expected"
    in
    let l = List.map snd l in
    let l = List.map get_arr l in
    let a, b = get_arr a in
    let eq = eq_var in
    (* Check that we have a unique path from b to a. *)
    let rec check seen l b =
      if l = [] then
        begin
          if not (eq a b) then failure b.pos "no producer for %s" (to_string b)
        end
      else
      if eq a b
      then failure pos "useless hypothesis: %s" (String.concat ", " @@ List.map (fun (a, b) -> to_string @@ mk (Arr (mk Obj, a, b))) l)
      else
        match List.find_and_remove_opt (fun (_,b') -> eq b b') l with
        | Some ((a, _), l) ->
          List.iter (fun (_,b') -> if eq b b' then failure b'.pos "multiple producers for %s" (to_string b)) l;
          List.iter (fun a' -> if eq a a' then failure a'.pos "cyclic dependencies %s already produced" (to_string a)) seen;
          check (a::seen) l a
        | None ->
          failure b.pos "no producer for %s" (to_string b)
    in
    check [] l b

  | `Cartesian_closed ->

    (* Turn products into arrows (the result is a formal product of types containing only arrows). *)
    let rec deproduct e =
      match e.desc with
      | Arr (o, a, b) when is_obj o ->
        let aa = deproduct a in
        let bb = deproduct b in
        List.map (fun b -> homs ~pos:e.pos aa b) bb
      | Hom (a, b) ->
        let aa = deproduct a in
        let bb = deproduct b in
        List.map (fun b -> homs ~pos:e.pos aa b) bb
      | Prod (a, b) -> (deproduct a)@(deproduct b)
      | One -> []
      | Var _
      | Obj -> [e]
      | _ -> failure e.pos "unhandled construction in deproduct: %s" (to_string e)
    in
    (* Given Γ⊢A, compute the deproduct of Γ⇒A. *)
    let aa =
      let l = List.map snd l in
      let l = List.map deproduct l |> List.flatten in
      List.map (homs ~pos l) (deproduct a)
    in
    (* printf "* check ccc: %s\n%!" (to_string a); *)
    let rec target a =
      match a.desc with
      | Var x -> x
      | Hom (_, b) -> target b
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
    (* Find the unique proof of a type c. vars is the list of head variables and env is the list of known arguments. *)
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
    List.iter (prove [] []) aa

  | `Cartesian ->

    (* printf "cartesian pasing scheme : %s ⊢ %s\n" (string_of_context l) (to_string a); *)
    let rec get_prod ?(distinct=false) a =
      let a0 = a in
      match (unmeta a).desc with
      | Prod (a, b) ->
        let a = get_prod a in
        let b = get_prod b in
        if distinct && not (VS.disjoint a b) then failure a0.pos "repeated variables";
        VS.union a b
      | One -> VS.empty
      | Var _ -> VS.singleton a
      | _ -> failure a.pos "product of variables expected"
    in
    let get_arr a =
      match (unmeta a).desc with
      | Arr (o, a, b) when is_obj o -> (a, b)
      | Arr (o, _, _) -> failure a.pos "1-dimensional arrow expected but got %s" (to_string o)
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
        match List.find_and_remove_opt (fun (a,_) -> VS.subset a available) l with
        | Some ((_,b),l) ->
          if not (VS.disjoint b available) then failure pos "variable produced multiple times: %s" (VS.to_seq b |> List.of_seq |> List.map to_string |> String.concat ", ");
          check (VS.union b available) l
        | None -> failure pos "some hypothesis could not be produced"
    in
    let available = check a l in
    if not (VS.subset b available) then failure pos "some variables cannot be produced: %s" (VS.diff b available |> VS.to_seq |> List.of_seq |> List.map to_string |> String.concat ", ")

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
      | Arr (o, a, b) when is_obj o ->
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
    (* NOTE: we could also check that the formula is balanced *)
    (* Make sure that sources and targets have distinct variables and are disjoint. *)
    List.iter (fun (a,b) ->
        let distinct a = List.iter_unordered_pairs (fun x y -> if eq_var x y then failure y.pos "repeated variable") a in
        distinct a;
        distinct b;
        (* NOTE: should already be handled by seen below *)
        (* List.iter (fun x -> List.iter (fun y -> if eq x y then failure y.pos "looping variable") b) a *)
      ) l;
    (* Make sure that sources and targets are pairwise disjoint. *)
    List.iter_unordered_pairs
      (fun (a,b) (a',b') ->
         let disjoint st a b = List.iter (fun x -> List.iter (fun y -> if eq_var x y then failure y.pos "variable already used in %s: %s" st (to_string y)) b) a in
         disjoint "source" a a';
         disjoint "target" b b'
      ) l;
    (* Make sure that we have a 2-path from b to a. *)
    let rec check seen l b =
      (* Printf.printf "check : %s\n%!" (to_string (prods b)); *)
      (* What remains of a after b *)
      let rec residual a b =
        match a, b with
        | x::a, y::b -> if eq_var x y then residual a b else None
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
      let rw = List.find_map_and_remove_opt rewrite l in
      match rw with
      | Some ((u, a, c), l) ->
        List.iter (fun x -> if List.exists (eq_var x) seen then failure pos "cyclic dependency on %s" (to_string x)) a;
        check (a@seen) l (u@a@c)
      | None ->
        if l <> [] then
          (
            let a, b = List.hd l in
            let a = String.concat " * " @@ List.map to_string a in
            let b = String.concat " * " @@ List.map to_string b in
            failure pos "unused rule %s -> %s" a b
          );
        if not (List.length a = List.length b && List.for_all2 eq_var a b) then failure pos "no producer for %s" (to_string (prods b))
    in
    check [] l b

  | `Symmetric_monoidal ->

    (* Make sure that the types are arrows between tensor expressions. *)
    let rec get_tens a =
      let pos = a.pos in
      match (unmeta a).desc with
      | Prod (a, b) ->
        let a = get_tens a in
        let b = get_tens b in
        if not (VS.disjoint a b) then failure pos "repeated variables";
        VS.union a b
      | One -> VS.empty
      | Var _ -> VS.singleton a
      | _ -> failure a.pos "tensor product of variables expected"
    in
    let get_arr a =
      match (unmeta a).desc with
      | Arr (o, a, b) when is_obj o ->
        let a' = get_tens a in
        let b' = get_tens b in
        if VS.is_empty a' then failure a.pos "type cannot be empty";
        if VS.is_empty b' then failure b.pos "type cannot be empty";
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
         if not (VS.disjoint a a') then failure pos "sources not disjoint";
         if not (VS.disjoint b b') then failure pos "targets not disjoint"
      ) l;
    (* Make sure that we have a 2-path from b to a. *)
    let rec check seen l b =
      (* Printf.printf "check : %s\n%!" (to_string (prods b)); *)
      let rule = List.find_and_remove_opt (fun (_,b') -> VS.subset b' b) l in
      match rule with
      | Some ((a',b'),l) ->
        if not (VS.disjoint a' seen) then failure pos "cyclic dependency";
        check (VS.union a' seen) l (VS.union (VS.diff b b') a')
      | None ->
        if l <> [] then
          (
            let a, b = List.hd l in
            let a = String.concat " * " @@ List.map to_string @@ VS.elements a in
            let b = String.concat " * " @@ List.map to_string @@ VS.elements b in
            failure pos "unused rule %s -> %s" a b
          );
        if not (VS.equal a b) then failure pos "no producer for %s" (to_string @@ prods @@ VS.elements b)
    in
    check VS.empty l b

  | `Symmetric_monoidal_closed ->

    let a = homs ~pos (List.map snd l) a in
    (* Check that the formula is balanced (every variable occurs at most once positively and at most once negatively. *)
    let rec balanced neg pos a =
      match a.desc with
      | Var _ -> if VS.mem a pos then failure a.pos "variable occurs twice with the same polarity"
      | One -> ()
      | Prod (a, b) -> balanced neg pos a; balanced neg pos b
      | Hom (a, b) -> balanced pos neg a; balanced neg pos b
      | _ -> assert false
    in
    balanced VS.empty VS.empty a;
    (* Prove a. *) 
    let rec prove env a =
      (* Printf.printf "prove: %s\n%!" (to_string a); *)
      match a.desc with
      | Var _ ->
        if not (List.exists (eq_var a) env) then failure a.pos "cannot produce %s" (to_string a);
        List.filter (fun b -> not (eq_var a b)) env
      | Prod (a, b) -> prove (prove env a) b
      | Hom (a, b) -> coprove a env b
      | _ -> failwith "TODO: smcc pasting, handle %s" (to_string a)
    (* Add a to the environment, simplifying it. *)
    and coprove a env b =
      match a.desc with
      | Var _ -> prove (a::env) b
      | Hom (a, a') -> coprove a' (prove env a) b
      | _ -> failure a.pos "TODO: smcc pasting (coprove), handle %s" (to_string a)
    in
    let env = prove (List.map snd l) a in
    if env <> [] then failure pos "unused hypothesis: %s" (env |> List.map to_string |> String.concat ", ")

  | `Linear_closed ->

    let rec target a =
      match a.desc with
      | Var _ -> a
      | Hom (_, b) -> target b
      | _ -> assert false
    in
    let rec prove env a =
      let pos = a.pos in
      match a.desc with
      | Var x ->
        (
          match List.find_and_remove_opt (fun b -> eq_var a (target b)) env with
          | Some (a', env) ->
            if List.exists (fun b -> eq_var a (target b)) env then failure pos "multiple producers for %s" x;
            coprove env a'
          | None -> failure pos "could not produce %s" x
        )
      | Hom (a, b) -> prove (a::env) b
      | _ -> assert false

    (* Prove all the hypothesis of the target of a. *)
    and coprove env a =
      match a.desc with
      | Var _ -> env
      | Hom (a, b) -> coprove (prove env a) b
      | _ -> assert false
    in
    let env = prove (List.map snd l) a in
    if env <> [] then failure pos "unused hypothesis: %s" (String.concat ", " @@ List.map to_string @@ env)

  | `Compact_closed ->

    (* We consider pairs of sets of variables: the positive ones and the negative ones. *)
    let union (a,a') (b,b') =
      if not (VS.disjoint a b) then failure pos "repeated variables: %s" (String.concat ", " @@ List.map to_string @@ VS.elements @@ VS.inter a b);
      if not (VS.disjoint a' b') then failure pos "repeated variables: %s" (String.concat ", " @@ List.map to_string @@ VS.elements @@ VS.inter a' b');
      VS.union a b, VS.union a' b'
    in
    let neg (a,a') = a',a in
    let rec get_tens a =
      match (unmeta a).desc with
      | Var _ -> VS.singleton a, VS.empty
      | One -> VS.empty, VS.empty
      | Prod (a, b) -> union (get_tens a) (get_tens b)
      | Op a -> neg @@ get_tens a
      | Arr (o, a, b) when is_obj o -> union (neg @@ get_tens a) (get_tens b)
      | _ -> failure a.pos "unhandled type: %s" @@ to_string a
    in
    let v, v' =
      let l = List.fold_left union (VS.empty,VS.empty) @@ List.map get_tens @@ List.map snd l in
      union (neg l) (get_tens a)
    in
    (* Make sure that every positive variable is matched with exactly one corresponding negative variable, and conversely. *)
    if not (VS.equal v v') then failure pos "non-matched variables: %s" (String.concat ", " @@ List.map to_string @@ VS.elements @@ VS.union (VS.diff v v') (VS.diff v' v));
    (* Make sure that we do not have cycles by constructing connected components. *)
    let cc = ref @@ List.map VS.singleton (VS.elements v) in
    List.iter
      (fun (_,a) ->
         let (a,a') = get_tens a in
         if not (VS.disjoint a a') then
           (
             let x = VS.choose @@ VS.inter a a' in
             failure x.pos "loop on %s" @@ to_string x
           );
         let v = VS.union a a' in
         let l,l' = List.partition (VS.disjoint v) !cc in
         if List.length l' <> VS.cardinal v then
           (
             ignore @@ failure pos "wrong cardinal";
             VS.iter
               (fun x ->
                  if List.count (VS.mem x) !cc > 1 then
                    failure x.pos "loop on %s" @@ to_string x
               ) v
           );
         let l' = List.fold_left VS.union VS.empty l' in
         cc := l' :: l
      ) l

(** Check whether a type in a context is a pasting scheme. We suppose that there are no variable declarations. *)
(* Here, we remove identities and call the above. *)
let check ~pos l a =

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
    | Arr (a, t, u) -> mk (Arr (rewrite a, rewrite t, rewrite u))
    | Hom (a, b) -> mk (Hom (rewrite a, rewrite b))
    | Prod (a, b) -> mk (Prod (rewrite a, rewrite b))
    | One -> mk One
    | Id (a, t, u) -> mk (Id (rewrite a, rewrite t, rewrite u))
    | Obj -> e
    | Op a -> mk (Op (rewrite a))
    | App (i, t, u) -> mk (App (i, rewrite t, rewrite u))
    | Coh (n, l, a, s) ->
      let s = List.map (fun (x,t) -> x, rewrite t) s in
      mk (Coh (n, l, a, s))
    | Meta { value = Some t; _ } -> rewrite t
    | Meta { value = None; _ } -> error ~pos:e.pos "unresolved metvariable %s when checking pasting conditions" (to_string e)
    | _ -> error ~pos:e.pos "TODO: in rewrite handle %s" (to_string e)
  in
  (* Add a rule to a rewriting system. *)
  let add_rule rw (x,t) =
    if List.exists (fun (y,_) -> x = y) rw then failure pos "we already have a rule on %s" x;
    (* printf "add rule %s -> %s\n" x (to_string t); *)
    let t = rewrite rw t in
    if has_fv x t then failure pos "cycle on %s" x;
    let rw = List.map (fun (y,u) -> y, rewrite [x,t] u) rw in
    (x,t)::rw
  in

  if !Settings.orientation = `Directed && Settings.has_elements () then
    (
      warning "orientation not supported yet for closed categories, falling back to reversible";
      Settings.set_orientation `Reversible
    );

  match !Settings.orientation with

  | `Reversible ->

    (* Remove identities/higher arrows in the context. *)
    let l, a =
      (* Orient identities on variables as rewriting rules and normalize l. *)
      (* TODO: we implicitly assume that all variable names are distinct in pasting schemes *)
      (* TODO: check that we are not a coboundary *)
      (* TODO: this makes us construct (∞,1)-categories, we should also investigate (∞,∞)-... *)
      let rec simplify rw l =
        let check (r, a) =
          let valid (x,t) =
            (* occurs check *)
            not (has_fv x t) &&
            (* the rule is not a boundary *)
            not (List.exists (fun (_,a) -> has_fv r a) l) &&
            (* there is no rewrite rule on this already *)
            not (List.exists (fun (y,_) -> x = y) rw)
          in
          match a.desc with
          | Id (_, {desc = Var x; _}, t) -> Some (x,t)
          | Id (_, t, {desc = Var x; _}) -> Some (x,t)
          | Arr (o, {desc = Var x; _}, t) when valid (x,t) && dim o >= 1 -> Some (x,t)
          | Arr (o, t, {desc = Var x; _}) when valid (x,t) && dim o >= 1 -> Some (x,t)
          | _ -> None
        in
        match List.find_map_and_remove_opt check l with
        | Some ((x,t),l) ->
          let l = List.filter (fun (x',_) -> x <> x') l in
          let rw = add_rule rw (x,t) in
          simplify rw l
        | None -> rw, l
      in
      let rw, l = simplify [] l in
      l, rewrite rw a
    in
    (* printf "**** after simplification: %s\n%!" (to_string (pis_explicit l a)); *)

    (* An identity is provable when its type is contractible. *)
    let a =
      let rec aux a =
        match a.desc with
        | Arr(a, _, _) when not (is_obj a) -> aux a
        | Id (a, _, _) -> aux a
        | _ -> a
      in
      aux a
    in

    check1 ~pos l a

  | `Directed ->

    let arr a =
      match (unmeta a).desc with
      | Arr (_, src, tgt) -> src, tgt
      | _ -> assert false
    in
    let pred a =
      match (unmeta a).desc with
      | Arr (a, _, _) -> a
      | _ -> assert false
    in
    let var x =
      match (unmeta x).desc with
      | Var x -> x
      | _ -> error ~pos:x.pos "variable expected, have %s" (to_string a)
    in
    (* Contract all rules above the maximal dimension, so that we are reversible. *)
    let l, a =
      let rw, l = List.partition (fun (_,a) -> dim a > !Settings.dimension) l in
      let rw = List.map (fun (_,a) -> let x, b = arr a in let x = var x in x, b) rw in
      let rw = List.fold_left add_rule [] rw in
      let l =
        let rw = List.map fst rw in
        List.filter (fun (x,_) -> not (List.mem x rw)) l
      in
      let l = List.map (fun (x,a) -> x, rewrite rw a) l in
      let a = rewrite rw a in
      l, a
    in
    let a =
      let rec aux a = if dim a > !Settings.dimension then aux (pred a) else a in
      aux a
    in
    (* printf "after contraction: %s\n%!" (to_string (pis_explicit l a)); *)
    let rec aux n l a =
      (* Printf.printf "aux %d : %s ⊢ %s\n%!" n (string_of_context l) (to_string a); *)
      if n <= 1 then check1 ~pos l a
      else
        (* Consider top dimensional cells as equations. *)
        let l, eq = List.partition (fun (_,a) -> dim a < n) l in
        let eq = List.map (fun (_,a) -> let x, y = arr a in var x, var y) eq in
        (* Note the set of equations should be acyclic, which is taken checked by add_rule *)
        let srcs = List.map fst eq in
        let tgts = List.map snd eq in
        let src = List.filter (fun (x,_) -> not (List.mem x tgts)) l in
        let tgt = List.filter (fun (x,_) -> not (List.mem x srcs)) l in
        (* Make sure that the source and target types are pasting. *)
        aux (n-1) src (pred a);
        aux (n-1) tgt (pred a);
        (* Make sure that the source target don't use removed variables (ideally, we should make sure that we can type in the source/target environment, but we cannot do that here). *)
        let vars =
          SS.union
            (SS.inter (fv @@ fst @@ arr a) (SS.of_list tgts))
            (SS.inter (fv @@ snd @@ arr a) (SS.of_list srcs))
        in
        (* Printf.printf "aux with %d : %s ⊢ %s\n%!" n (string_of_context l) (to_string a); *)
        if not (SS.is_empty vars) then failure pos "not allowed to use those variables in source / target: %s" (String.concat ", " @@ SS.elements vars)
    in
    let n = dim a in
    List.iter (fun (_,a) -> if dim a > n then failure a.pos "type %s has dimension %d but trying to construct a term of dimension %d" (to_string a) (dim a) n) l;
    aux n l a

(** Check whether a type in a context is a pasting scheme. *)
(* Here, we cleanup the variable declarations and call the above. *)
let check ~pos l a =
  (* printf "* check_ps: %s\n%!" (to_string (pis_explicit l a)); *)

  (* Remove variable declarations from the context. *)
  let vars, l =
    let split_vars l =
      let is_obj a = a.desc = Obj in
      let vars = List.fold_left (fun vars (x,a) -> if is_obj a then SS.add x vars else vars) SS.empty l in
      let l = List.filter (fun (_,a) -> not (is_obj a)) l in
      vars, l
    in
    split_vars l
  in
  (* printf "**** without variables: %s\n%!" (to_string (pis_explicit l a)); *)

  (* Ensure that the declared variables are exactly the free variables. *)
  let () =
    let a = homs ~pos (List.map snd l) a in
    let d = SS.diff vars (fv a) in
    if not (SS.is_empty d) then failure a.pos "unused variables: %s" (String.concat ", " @@ SS.elements d)
  in

  check ~pos l a

let check ~pos l a =
  (* In case we explicitely disable checking everything is accepted as a pasting scheme. *)
  if not !Settings.disable_pasting_check then check ~pos l a
