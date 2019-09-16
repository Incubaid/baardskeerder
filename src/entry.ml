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

(* .. *)
open Base
open Leaf
open Index
open Indexz
open Commit
open Leafz


type entry =
  | NIL
  | Value of v
  | Leaf of leaf
  | Index of index
  | Commit of commit


type dir =
  | Leaf_down of leaf_z
  | Index_down of index_z

type path = dir list

let entry2s = function
  | NIL -> "NIL"
  | Value v  -> Printf.sprintf "Value \"%s\"" v
  | Leaf l   -> Printf.sprintf "Leaf %s" (leaf2s l)
  | Index i  -> Printf.sprintf "Index %s" (index2s i)
  | Commit c -> Printf.sprintf "Commit %s" (commit2s c)

let string_of_dir = function
  | Leaf_down l  -> Printf.sprintf "Leaf_down %s" (lz2s l)
  | Index_down i -> Printf.sprintf "Index_down (%s)" (iz2s i)

let wrong expected e = Printf.sprintf "%s is not a %s" (entry2s e) expected |> failwith

let get_value = function
  | Value v -> v
  | NIL | Leaf _ | Index _ |  Commit _ as e -> wrong "value" e

let get_commit = function
  | Commit c -> c
  | NIL | Value _ | Leaf _ | Index _  as e -> wrong "commit" e
