(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2011 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 *)

type t
type slab

val create : string -> unit
val make : string -> t
val close : t -> unit
val sync : t -> unit
val compact : ?min_blocks:int -> t -> unit

val clear : t -> unit

val read : t -> int -> Entry.entry
val next : t -> int
val last : t -> int

val write : t -> slab -> unit

val make_slab : t -> slab
val add : slab -> Entry.entry -> Base.pos
val string_of_slab : slab -> string
