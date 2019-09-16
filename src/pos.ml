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

type spindle = int
type offset = int

type pos =
  | Outer of spindle * offset
  | Inner of int

let out s o = Outer (s, o)

let get_out = function
  | Outer (s, o) -> (s,o)
  | Inner _ -> failwith "expected Outer, not Inner"

let get_in = function
  | Inner p -> p
  | Outer _ -> failwith "expected Inner, not Outer"

let pos2s = function
  | Outer (s, o) -> Printf.sprintf "Outer (%d, %d)" s o
  | Inner p -> Printf.sprintf "Inner %i" p

let next = function
  | Inner i -> Inner  (i + 1)
  | Outer _ -> failwith "Outer positions have no next"
