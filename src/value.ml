(** Values (i.e. evaluated expressions). *)

open Extlib
open Common

(** A value. *)
type t =
  | Abs of (t -> t)
  | Obj
  | Hom of t * t
  | Prod of t * t
  | Pi of t * (t -> t) (** a meta-arrow *)
  | Id of t * t
  | Type
  | Meta of meta (** a metavariable *)
  | Neutral of neutral

and meta =
  {
    pos : Pos.t option;
    id : int;
    mutable value : t option;
  }

(** A neutral value. *)
and neutral =
  | Coh
  | Var of int
  | App of neutral * t

let var k = Neutral (Var k)

let metavariable =
  let id = ref 0 in
  fun ?pos () ->
    let m =
      incr id;
      let id = !id in
      { pos; id; value = None }
    in
    Meta m

let rec to_string k ?(pa=false) t =
  let pa s = if pa then "(" ^ s ^ ")" else s in
  match t with
  | Abs t ->
    let x = var k in
    Printf.sprintf "fun %s -> %s" (to_string k x) (to_string (k+1) (t x)) |> pa
  | Obj -> "."
  | Hom (a, b) -> Printf.sprintf "%s -> %s" (to_string ~pa:true k a) (to_string k b) |> pa
  | Prod (a, b) -> Printf.sprintf "%s * %s" (to_string ~pa:true k a) (to_string k b) |> pa
  | Pi (a, b) ->
    let x = var k in
    Printf.sprintf "(%s : %s) => %s" (to_string k x) (to_string k a) (to_string (k+1) (b x)) |> pa
  | Id (a, b) -> Printf.sprintf "%s = %s" (to_string k a) (to_string k b) |> pa
  | Type -> "Type"
  | Meta m ->
    (
      match m.value with
      | Some t -> Printf.sprintf "[%s]" (to_string k t)
      | None -> Printf.sprintf "?%d" m.id
    )
  | Neutral n ->
    let rec aux = function
      | Coh -> "coh"
      | Var i ->
        (* Printf.sprintf "x%d" i *)
        String.make 1 (char_of_int (int_of_char 'a' + i))
      | App (n, _) -> aux n
    in
    aux n

let rec metavariables = function
  | Meta m -> [m]
  | Obj
  | Type -> []
  | Hom (a, b)
  | Prod (a, b)
  | Id (a, b) -> metavariables a @ metavariables b
  | Abs t -> metavariables (t (var 0))
  | Pi (a, b) -> metavariables a @ metavariables (b (var 0))
  | Neutral n ->
    let rec aux = function
      | Coh
      | Var _ -> []
      | App (n, a) -> aux n @ metavariables a
    in
    aux n

(** String representation of a value. *)
let to_string ?(k=0) = to_string k

let print_metavariables_elaboration t =
  metavariables t |> List.iter
    (fun m ->
       let v =
         match m.value with
         | Some v -> to_string v
         | None -> "?"
       in
       if m.pos <> None then printf "... at %s, ?%d elaborated to %s\n" (Pos.to_string (Option.get m.pos)) m.id v
    )

let rec homs l a =
  match l with
  | b::l -> Hom (b, homs l a)
  | [] -> a

exception Unification

(** Make sure that two values are equal (and raise [Unification] if this cannot be the case). *)
let rec unify k t t' =
  (* Printf.printf "unify %s with %s\n" (to_string t) (to_string t'); *)
  let rec neutral k t t' =
    match t, t' with
    | App (t, u), App (t', u') -> neutral k t t'; unify k u u'
    | Var i, Var j -> if i <> j then raise Unification
    | Coh, Coh -> ()
    | _ -> raise Unification
  in
  match t, t' with
  | Neutral t, Neutral u -> neutral k t u
  | Hom (a, b), Hom (a' , b') -> unify k a a'; unify k b b'
  | Prod (a, b), Prod (a' , b') -> unify k a a'; unify k b b'
  | Abs t, Abs t' -> let x = var k in unify (k+1) (t x) (t' x)
  | Pi (a, t), Pi (a', t') -> let x = var k in unify k a a'; unify (k+1) (t x) (t' x)
  | Obj, Obj
  | Type, Type -> ()
  | Meta { value = Some t; _ }, t' -> unify k t t'
  | t, Meta { value = Some t'; _ } -> unify k t t'
  | Meta m, t
  | t, Meta m ->
    (* printf "... ?%d becomes %s\n" m.id (to_string t); *)
    m.value <- Some t
  | _ -> raise Unification
