(** Values are evaluated expressions. *)

(** A value. *)
type t =
  | Abs of (t -> t)
  | Obj
  | Hom of t * t
  | Prod of t * t
  | Pi of t * (t -> t) (** a meta-arrow *)
  | Id of t * t
  | Type
  | Neutral of neutral

(** A neutral value. *)
and neutral =
  | Coh
  | Var of int
  | App of neutral * t

let var k = Neutral (Var k)

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
  | Neutral n ->
    let rec aux = function
      | Coh -> "coh"
      | Var i ->
        (* Printf.sprintf "x%d" i *)
        String.make 1 (char_of_int (int_of_char 'a' + i))
      | App (n, _) -> aux n
    in
    aux n

(** String representation of a value. *)
let to_string ?(k=0) = to_string k

let rec homs l a =
  match l with
  | b::l -> Hom (b, homs l a)
  | [] -> a

(** Test for equality of values (default equality should never be used on values). *)
let rec eq k t t' =
  let rec neutral_eq k t t' =
    match t, t' with
    | App (t, u), App (t', u') -> neutral_eq k t t' && eq k u u'
    | Var i, Var j -> i = j
    | Coh, Coh -> true
    | _ -> false
  in
  match t, t' with
  | Neutral t, Neutral u -> neutral_eq k t u
  | Hom (a, b), Hom (a' , b') -> eq k a a' && eq k b b'
  | Prod (a, b), Prod (a' , b') -> eq k a a' && eq k b b'
  | Obj, Obj
  | Type, Type -> true
  | _ -> false
