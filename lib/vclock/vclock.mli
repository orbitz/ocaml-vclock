module Ordering : sig
  type t =
    | Lt
    | Gt
    | Eq
    | Concurrent
end


module type SITE = sig
  type t
  val equal : t -> t -> bool
end

module Make : functor (Site : SITE) -> sig
  type t

  val empty     : t
  val compare   : t -> t -> Ordering.t
  val increment : Site.t -> t -> t
  val merge     : t -> t -> t
  val prune     : (Site.t -> bool) -> t -> t
end
