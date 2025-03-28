(** Terms of our language. *)

open Extlib
open Common

(** An expression. *)
type t =
  {
    desc : desc;
    pos : Pos.t;
  }

and desc =
  | Coh  of string * (icit * string * t) list * t * substitution (** coherence *)
  | Var  of string (** variable *)
  | Abs  of icit * string * t * t (** abstraction *)
  | App  of icit * t * t (** application *)
  | Pi   of icit * string * t * t (** Π-type *)
  | Id   of t * t * t (** identity type *)
  | Arr  of t * t * t (** arrow type *)
  | Hom  of t * t (** internal hom type *)
  | Prod of t * t (** product type *)
  | Op   of t (** opposite of a type *)
  | One (** terminal type *)
  | Meta of meta (** a variable to be unified *)
  | Obj  (** object type *)
  | Type (** the type of types *)

and icit = [`Explicit |  `Implicit]

and context = (string * t) list

and substitution = (string * t) list

and meta =
  {
    id : int;
    source_pos : Pos.t option; (** position in the source code (can be empty if the metavariable was created on the fly) *)
    mutable value : t option;
    ty : t;
  }

let rec is_pi e =
  match e.desc with
  | Pi _ -> true
  | Meta m when m.value <> None -> is_pi @@ Option.get m.value
  | _ -> false

(** String representation of an expression. This should mostly be useful for debugging (we want to print values). *)
let rec to_string ?(pa=false) e =
  let rec dim e =
    match e.desc with
    | Obj -> 0
    | Arr (a, _, _) -> 1 + dim a 
    | _ -> 0
  in
  let pa s = if pa then "(" ^ s ^ ")" else s in
  match e.desc with
  (* | Coh (l, a) -> Printf.sprintf "coh[%s|%s]" (List.map (fun (x,a) -> Printf.sprintf "%s:%s" x (to_string a)) l |> String.concat ",") (to_string a) *)
  (* | Coh _ -> "coh" *)
  (* | Coh (n,_,_,s) -> Printf.sprintf "%s[%s]" n (String.concat ", " @@ List.map (fun (x,t) -> Printf.sprintf "%s=%s" x (to_string t)) s) *)
  | Coh (n,l,_,s) -> Printf.sprintf "%s[%s]" n (String.concat "," @@ List.filter_map Fun.id @@ List.map2 (fun (i,_,_) (_,t) -> if i = `Explicit then Some (to_string t) else None) l s)
  | Var x -> x
  | Abs (i, x, a, t) ->
    if i = `Implicit then
      Printf.sprintf "fun {%s : %s} ⤳ %s" x (to_string a) (to_string t) |> pa
    else
      Printf.sprintf "fun (%s : %s) ⤳ %s" x (to_string a) (to_string t) |> pa
  | App (i, t, u) ->
    let isnt_app e = match e.desc with App _ -> false | _ -> true in
    if i = `Implicit then Printf.sprintf "%s {%s}" (to_string ~pa:(isnt_app t) t) (to_string u) |> pa
    else Printf.sprintf "%s %s" (to_string ~pa:(isnt_app t) t) (to_string ~pa:true u) |> pa
  | Pi (i, x, a, t) ->
    let arr = if is_pi t then "" else " ⤳" in
    if i = `Implicit then
      Printf.sprintf "{%s : %s}%s %s" x (to_string a) arr (to_string t) |> pa
    else
      Printf.sprintf "(%s : %s)%s %s" x (to_string a) arr (to_string t) |> pa
  | Obj -> "."
    
  | Arr (a, t, u) -> Printf.sprintf "%s %s %s" (to_string ~pa:true t) (if dim a = !Settings.dimension then "=" else "→") (to_string u) |> pa
  | Hom (a, b) -> Printf.sprintf "%s ⇒ %s" (to_string ~pa:true a) (to_string b) |> pa
  | Prod (a, b) -> Printf.sprintf "%s × %s" (to_string ~pa:true a) (to_string b) |> pa
  | One -> "1"
  | Op a -> "! " ^ to_string ~pa:true a
  | Id (a, t, u) ->
    Printf.sprintf "(%s = %s : %s)" (to_string ~pa:true t) (to_string ~pa:true u) (to_string ~pa:true a) |> pa
  | Meta m ->
    (
      match m.value with
      | Some t -> to_string t
      | _ -> Printf.sprintf "?%d" m.id
    )
  | Type -> "Type"

let string_of_icit = function
  | `Implicit -> "implicit"
  | `Explicit -> "explicit"

let string_of_context env = List.map (fun (x,v) -> x ^ " : " ^ to_string v) env |> String.concat ","

(** Create an expression from its contents. *)
let mk ?pos desc : t =
  (* assert (pos <> None); *)
  let pos = Option.value ~default:Pos.dummy pos in
  { desc; pos }

(** Change the position. *)
let repos pos e = mk ~pos e.desc

let var ?pos x = mk ?pos (Var x)

let meta_counter = ref 0

let meta ?pos ?(real=false) ?value ty =
  incr meta_counter;
  mk ?pos (Meta { source_pos = if real then pos else None; value; id = !meta_counter; ty })

(* Real means that the hole was written in the original source (so that we should display its elaborated contents). *)
let hole ?pos ?(real=false) () =
  meta ?pos ~real (meta ?pos (mk ?pos Type))

(** Replace metavariables at toplevel by their content. *)
let rec unmeta e =
  match e.desc with
  | Meta { value = Some e; _ } -> unmeta e
  | _ -> e

(** Create coherence with abstracted arguments. *)
let abs_coh ?pos name l a =
  let mk = mk ?pos in
  let rec aux = function
    | (i,x,a)::l -> mk (Abs (i,x, a, aux l))
    | [] ->
      let s = List.map (fun (_,x,_) -> x, var ?pos x) l in
      mk (Coh (name, l, a, s))
  in
  aux l

(** Commands a toplevel actions. *)
type command =
  | Let of string * t option * t (** declare a value *)
  | NCoh of string * context * t (** ensure that we are *not* coherent *)
  | Include of string (** include another file *)
  | Setting of string (** change a setting *)

(** A program. *)
type prog = command list

let fresh_var_name =
  let h = Hashtbl.create 100 in
  fun x ->
    let n = Option.value ~default:0 @@ Hashtbl.find_opt h x in
    Hashtbl.replace h x (n+1);
    x ^ "#" ^ string_of_int n

let fresh_var x = fresh_var_name x |> var

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

(** Build multiple pi types. *)
let pis_explicit ?pos l t =
  pis ?pos (List.map (fun (x,a) -> `Explicit,x,a) l) t

(** Build multiple products. *)
let rec prods ?pos l =
  match l with
  | [x] -> x
  | x::l -> mk ?pos (Prod (x, prods ?pos l))
  | [] -> mk ?pos One

(* Test whether an expression has a free variable. *)
let rec has_fv x e =
  match e.desc with
  | Var y -> x = y
  | Hom (a, b)
  | Prod (a, b) -> has_fv x a || has_fv x b
  | Arr (a, t, u)
  | Id (a, t, u) -> has_fv x a || has_fv x t || has_fv x u
  | App (_, t, u) -> has_fv x t || has_fv x u
  | Meta { value = Some t; ty = ty; _ } -> has_fv x t || has_fv x ty
  | Meta { value = None; ty = ty; _ } -> has_fv x ty
  | One
  | Obj
  | Type -> false
  | Coh (_,_,_,s) -> List.exists (fun (_,t) -> has_fv x t) s
  | _ -> error ~pos:e.pos "has_fv: handle %s" (to_string e)

let is_var e =
  match (unmeta e).desc with
  | Var _ -> true
  | _ -> false

let is_obj e =
  match (unmeta e).desc with
  | Obj -> true
  | _ -> false

let is_implicit_pi e =
  match e.desc with
  | Pi(`Implicit,_,_,_) -> true
  | _ -> false

let is_metavariable e =
  match e.desc with
  | Meta { value = None; _ } -> true
  | _ -> false

(** All metavariables of a term. *)
let metavariables e =
  let rec aux e acc =
    match e.desc with
    | Coh (_, _, _, s) -> List.fold_left (fun acc (_,t) -> aux t acc) acc s
    | Var _ -> acc
    | Abs (_, _, a, t) -> acc |> aux a |> aux t
    | App (_, t, u) -> acc |> aux t |> aux u
    | Obj -> acc
    | Pi (_, _, a, b)
    | Hom (a, b)
    | Prod (a, b) -> acc |> aux a |> aux b
    | One -> acc
    | Arr (a, t, u)
    | Id (a, t, u) -> acc |> aux a |> aux t |> aux u
    | Op a -> acc |> aux a
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

(** Make sure that two variables are equal. *)
let eq_var a b =
  match (unmeta a).desc, (unmeta b).desc with
  | Var a, Var b -> a = b
  | _ -> assert false

let compare_var a b =
  match (unmeta a).desc, (unmeta b).desc with
  | Var a, Var b -> compare a b
  | _ -> assert false

let failure pos fmt =
  Printf.ksprintf (fun s -> failwith "%s: %s" (Pos.to_string pos) s) fmt

let rec dim e =
  match (unmeta e).desc with
  | Obj -> 0
  | Arr (a, _, _) -> 1 + dim a
  | _ -> assert false

let free_variable_names a =
  let rec aux a fv =
    match (unmeta a).desc with
    | Var x -> x::fv
    | Obj -> fv
    | Arr (a, t, u) -> fv |> aux a |> aux t |> aux u
    | Hom (a, b) -> fv |> aux a |> aux b
    | Prod (a, b) -> fv |> aux a |> aux b
    | One -> fv
    | Op a -> fv |> aux a
    | Id (a, t, u) -> fv |> aux a |> aux t |> aux u
    | App (_, t, u) -> fv |> aux t |> aux u
    | Coh (_,_,_,s) -> List.fold_left (fun fv (_,t) -> aux t fv) fv s
    | _ -> failure a.pos "TODO: fv handle %s" (to_string a)
  in
  aux a []
