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

open Entry
open Base


module type LOG = sig
  type t
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m

  val init: ?d:int -> string  -> Time.t -> unit m
  val write : t -> Slab.t -> unit m
  val last  : t -> pos
  val lookup: t -> pos m
  val read  : t -> pos -> entry m
  val sync  : t -> unit m
  val make  : string -> t m
  val close : t -> unit m
  val clear: t -> unit m
  val get_d: t -> int
  val now: t -> Time.t
  val dump: ?out:Pervasives.out_channel -> t -> unit m
  val compact: ?min_blocks:int ->
    ?progress_cb:(offset -> offset -> unit) option -> t -> unit m

  val set_metadata: t -> string -> unit m
  val get_metadata: t -> string option m
  val unset_metadata: t -> unit m
end
