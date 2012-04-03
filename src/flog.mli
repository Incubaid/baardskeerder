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

type 'a m
val bind : 'a m -> ('a -> 'b m) -> 'b m
val return : 'a -> 'a m
val run : 'a m -> 'a

type t

val init : ?d:int -> string -> Time.t -> unit m
val make : string -> t m
val close : t -> unit m
val sync : t -> unit m
val compact : ?min_blocks:int ->
  ?progress_cb:(Pos.offset -> Pos.offset -> unit) option -> t -> unit m

val clear : t -> unit m

val read : t -> Pos.pos -> Entry.entry m
val last : t -> Pos.pos

val write : t -> Slab.t -> unit m
val get_d : t -> int
val dump : ?out:out_channel -> t -> unit m

val now : t -> Time.t
