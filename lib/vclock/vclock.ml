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

let clock_max c1 c2 =
  if c1.n < c2.n then
    c2
  else
    c1

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

  (*
   * Compare two orderings and return the least
   * strong ordering of the two
   *)
  let ord r1 r2 =
    match (r1, r2) with
      | (Ordering.Eq, rel)    -> rel
      | (rel, Ordering.Eq)    -> rel
      | (r1, r2) when r1 = r2 -> r1
      | (_, _)                -> Ordering.Concurrent

  (*
   * If a site for a clock is in another vector clock
   * then compare the n value's and the ordering depends
   * on how that compares.
   * If it is not, then the clock entry indicates that the
   * vclock it came from is newer than the one we're comparing
   * to, thus it is greater than
   *)
  let compare_clocks ordering c1 t =
    match take (fun c -> Site.equal c1.s c.s) t with
      | Some (c2, t) ->
	let o = ord ordering (compare_n c1.n c2.n) in
	(o, t)
      | None ->
	let o = ord ordering Ordering.Gt in
	(o, t)

  (*
   * To compare vector clocks we maintain an ordering
   * and then update the ordering until we get to an end.
   * We start assuming the two vclocks are equal, then
   * update the ordering as we go.  The first 3 cases are
   * pretty obvious, the tricky one is if we have two
   * vclocks with values in it, in which case we take a
   * clock from one of the vclocks, compare that to the
   * associated clock in the other vclock, update the
   * ordering, then compare the remainder of the vclocks
   *)
  let rec compare_rel ordering t1 t2 =
    match (t1, t2) with
      | (_::_,   []  ) -> ord ordering Ordering.Gt
      | ([],     _::_) -> ord ordering Ordering.Lt
      | ([],     []  ) -> ordering
      | (c::cs, t) -> begin
	let (ordering, rest) = compare_clocks ordering c t in
	compare_rel ordering cs rest
      end

  let compare = compare_rel Ordering.Eq

  (*
   * Find the site in the vclock and update it otherwise
   * add it to the vclock.  The site passed in is the one
   * the update uses, this way if there is more data
   * encoded in the site it will be the latest version
   * (hint: encoding timestamps for pruning)
   *)
  let rec increment s = function
    | [] ->
      [{ s = s; n = 1}]
    | c::cs when Site.equal c.s s ->
      {c with s = s; n = c.n + 1}::cs
    | c::cs ->
      c::(increment s cs)

  (*
   * For a clocks in two vclocks merge them together.  If
   * a clock is in both vclocks, take the clock with the highest
   * n value
   *)
  let rec merge t1 t2 =
    match (t1, t2) with
      | ([], t2)    -> t2
      | (t1, [])    -> t1
      | (c::cs, t2) -> begin
	match find (fun clock -> Site.equal clock.s c.s) t2 with
	  | Some clock ->
	    (clock_max c clock)::(merge cs t2)
	  | None ->
	    c::(merge cs t2)
      end

  let prune f t =
    List.filter (fun c -> f c.s) t

  let to_list = List.map (fun c -> (c.s, c.n))
  let from_list = List.map (fun (s, n) -> {s; n})

end
