module type DB = sig 
  type t
  type k:string
  type v:string

  val set : t -> k -> v -> unit
  val get : t -> k -> v
  val get': t -> k -> (v -> 'a) -> 'a
  val del : t -> k -> unit
end
