(** Our own extension of the standard library. *)

(** Lists. *)
module List = struct
  include List

  (** Like assoc, but key is the second component. *)
  let rec assoc' x = function
    | (v,y)::_ when x = y -> v
    | _::l -> assoc' x l
    | [] -> raise Not_found

  (** Count the number of elements satisfying a predicate. *)
  let rec count p = function
    | x::l -> if p x then 1 + count p l else count p l
    | [] -> 0

  let rec find_and_remove_opt p = function
    | x::l ->
      if p x then Some (x, l)
      else Option.map (fun (y, l) -> y, x::l) (find_and_remove_opt p l)
    | [] -> None

  let rec find_map_and_remove_opt f = function
    | x::l ->
      (
        match f x with
        | Some x' -> Some (x', l)
        | None -> Option.map (fun (y, l) -> y, x::l) (find_map_and_remove_opt f l)
      )
    | [] -> None

  let included l1 l2 =
    List.for_all (fun x -> mem x l2) l1

  let diff l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

  let rec exists_unique p = function
    | x::l when p x -> List.for_all (fun x -> not (p x)) l
    | _::l -> exists_unique p l
    | [] -> false

  let remove x l = List.filter (fun y -> x <> y) l

  let rec iter_unordered_pairs f = function
    | x::l ->
      List.iter (f x) l;
      iter_unordered_pairs f l
    | [] -> ()
    
end

(** Positions in the source code. *)
module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy = Lexing.dummy_pos, Lexing.dummy_pos

  let lexeme lexbuf : t = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf

  let union (p1,p2) (q1,q2) =
    assert (p1.Lexing.pos_fname = q1.Lexing.pos_fname);
    let r1 = if p1.Lexing.pos_cnum <= q1.Lexing.pos_cnum then p1 else q1 in
    let r2 = if p2.Lexing.pos_cnum >= q2.Lexing.pos_cnum then p2 else q2 in
    r1,r2

  (** String representation of a position. *)
  let to_string ((p1,p2):t) =
    let l1 = p1.Lexing.pos_lnum in
    let l2 = p2.Lexing.pos_lnum in
    let b1 = p1.Lexing.pos_bol in
    let b2 = p2.Lexing.pos_bol in
    let c1 = p1.Lexing.pos_cnum in
    let c2 = p2.Lexing.pos_cnum in
    let c1 = c1 - b1 in
    let c2 = c2 - b2 in
    (
      if p1.Lexing.pos_fname <> "" then
        Printf.sprintf "in file %s " p1.Lexing.pos_fname
      else
        ""
    ) ^
      if l1 = l2 then
        if c1 = c2 then
          Printf.sprintf "line %d character %d" l1 c1
        else
          Printf.sprintf "line %d characters %d-%d" l1 c1 c2
      else
        Printf.sprintf "from line %d character %d to line %d character %d" l1 c1 l2 c2

  module Option = struct
    type nonrec t = t option

    let to_string = function
      | Some pos -> to_string pos
      | None -> "unknown position"
  end
end

module String = struct
  include String

  let split_on_first_char c s =
    let n = String.index s c in
    String.sub s 0 n , String.sub s (n+1) (String.length s - n-1)
end
