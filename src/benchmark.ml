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

open Unix
open Tree

let clock f = 
  let t0 = Unix.gettimeofday () in
  let () = f() in
  let t1 = Unix.gettimeofday () in
  t1 -. t0

module FDB = DB(Flog)

let make_key i = Printf.sprintf "key_%08i" i 

let set_loop db vs n = 
  let v = String.make vs 'x' in
  let set k v = FDB.set db k v in
  let rec loop i = 
    if i = n 
    then ()
    else
      let key = make_key i in
      let () = set key v in
      loop (i+1)
  in
  loop 0

let get_loop db n = 
  let get k = FDB.get db k in
  let rec loop i =
    if i = n 
    then ()
    else
      let key = make_key i in
      let v = get key in
      loop (i+1)
  in
  loop 0

let delete_loop db n = 
  let delete k = FDB.delete db k in
  let rec loop i = 
    if i = n 
    then ()
    else
      let key = make_key i in
      let () = delete key in
      loop (i+1)
  in
  loop 0

let () = 
  let n = 
    if Array.length Sys.argv = 2 
    then int_of_string Sys.argv.(1)
    else
      1_000_000
  in
  let vs = 1000 in
  let fn = "test.db" in
  let () = Flog.create fn 4096 in
  let db = Flog.make fn in
  let () = Printf.printf "n=%i iterations; value size=%i\n%!" n vs in
  let d = clock (fun () -> set_loop db vs n) in
  Printf.printf "%i sets of (%i bytes) took:%fs\n%!" n vs d;
  let d2 = clock (fun () -> get_loop db n) in
  Printf.printf "%i gets took:%fs\n%!" n d2;
  let d3 = clock (fun () -> delete_loop db n) in
  Printf.printf "%i deletes took:%fs\n%!" n d3;
  let () = Flog.close db in
  ();;
