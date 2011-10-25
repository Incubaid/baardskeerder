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

let set_loop db vs n = 
  let v = String.make vs 'x' in
  let set k v = () in
  let rec loop i = 
    if i = n then ()
    else
      let key = Printf.sprintf "key_%08i" i in 
      let () = FDB.set db key v in
      loop (i+1)
  in
  loop 0;;

  


let () = 
  let n = 100000 in
  let vs = 1000 in
  let fn = "test.db" in
  let () = Flog.create fn 4096 in
  let db = Flog.make fn in
  let d = clock (fun () -> set_loop db vs n) in
  let () = Flog.close db in
  Printf.printf "%n sets of (%i bytes) took:%f\n" n vs d;;
