module List = struct
  include List

  let rec find_and_remove_opt p = function
    | x::l ->
      if p x then Some (x, l)
      else Option.map (fun (y, l) -> y, x::l) (find_and_remove_opt p l)
    | [] -> None

  let included l1 l2 =
    List.for_all (fun x -> mem x l2) l1

  let diff l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1
end

(** Position in source code. *)
module Pos = struct
  type t = Lexing.position * Lexing.position

  let dummy = Lexing.dummy_pos, Lexing.dummy_pos

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
end
