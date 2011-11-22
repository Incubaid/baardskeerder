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
val make : unit -> t
val string_of_slab : t -> string
val add : t -> Entry.entry -> Pos.pos
val length : t -> int
val iter_rev : (Entry.entry -> unit) -> t -> unit

val rev_es : t -> Entry.entry list
val mark: t -> bool array
val mapping: bool array -> (int,int) Hashtbl.t (* not abstract enough *)
