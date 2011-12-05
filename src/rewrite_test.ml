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

open Mlog
open Rewrite
open Tree
open OUnit

module MMRewrite = Rewrite (Mlog) (Mlog)
module MDB = DB(Mlog)

let test_presence () =
  let kvs = 
    let rec loop acc = function 
      | 26 -> acc
      | i -> 
	let c0 = Char.chr (65 + i) in
	let c1 = Char.chr (96 + i) in
	let kv = (Printf.sprintf "%c" c0, Printf.sprintf "%c" c1) in
	     loop (kv :: acc) (i+1)
    in
    loop [] 0
  in
  let m0 = Mlog.make "m0" in
  let m1 = Mlog.make "m1" in
  List.iter (fun (k,v) -> MDB.set m0 k v) kvs;
  let root0 = Mlog.last m0 in
  let () = MMRewrite.rewrite m0 root0 m1 in
  let now = Mlog.now m0 in
  let fut = Time.next_major now in
  let empty = Slab.make fut in 
  let () = List.iter (fun (k,v) -> let v' = MDB.get m1 empty k in OUnit.assert_equal v' v) kvs in
  ()


let suite = "Rewrite" >::: [
  "presence" >:: test_presence
]
