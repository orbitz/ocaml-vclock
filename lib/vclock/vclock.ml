module Ordering = struct
  type t =
    | Lt
    | Gt
    | Eq
    | Concurrent
end


(*
 * Some helpful functions
 *)
let max i1 i2 =
  if i1 < i2 then
    i2
  else
    i1

let find f x =
  try
    Some (List.find f x)
  with
      Not_found ->
	None

let take f =
  let rec take' acc = function
    | [] ->
      None
    | x::xs when f x ->
      Some (x, List.rev acc @ xs)
    | x::xs ->
      take' (x::acc) xs
  in
  take' []

let compare_n n1 n2 =
  match (n1, n2) with
    | (n1, n2) when n1 < n2 -> Ordering.Lt
    | (n1, n2) when n1 > n2 -> Ordering.Gt
    | _                     -> Ordering.Eq

module type SITE = sig
  type t
  val equal : t -> t -> bool
end


module Make = functor (Site : SITE) -> struct
  type clock = { s : Site.t
	       ; n : int
	       }

  type t = clock list

  let empty = []

  let rel r1 r2 =
    match (r1, r2) with
      | (Ordering.Eq, rel)    -> rel
      | (rel, Ordering.Eq)    -> rel
      | (r1, r2) when r1 = r2 -> r1
      | (_, _)                -> Ordering.Concurrent

  let compare_clocks r c1 t =
    match take (fun c -> Site.equal c1.s c.s) t with
      | Some (c2, t) ->
	let r = rel r (compare_n c1.n c2.n) in
	(r, t)
      | None ->
	let r = rel r Ordering.Gt in
	(r, t)

  let rec compare_rel r t1 t2 =
    match (t1, t2) with
      | (_::_,   []  ) -> rel r Ordering.Gt
      | ([],     _::_) -> rel r Ordering.Lt
      | ([],     []  ) -> r
      | (c::cs, t) -> begin
	let (rel, rest) = compare_clocks r c t in
	compare_rel rel cs rest
      end

  let compare = compare_rel Ordering.Eq

  let rec increment s = function
    | [] ->
      [{ s = s; n = 1}]
    | c::cs when Site.equal c.s s ->
      {c with n = c.n + 1}::cs
    | c::cs ->
      c::(increment s cs)

  let rec merge t1 t2 =
    match (t1, t2) with
      | ([], t2)    -> t2
      | (t1, [])    -> t1
      | (c::cs, t2) -> begin
	match find (fun clock -> Site.equal clock.s c.s) t2 with
	  | Some clock ->
	    {clock with n = max c.n clock.n}::(merge cs t2)
	  | None ->
	    c::(merge cs t2)
      end

  let prune f t =
    List.filter (fun c -> f c.s) t

end
