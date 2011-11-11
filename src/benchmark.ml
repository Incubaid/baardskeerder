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
open Arg

let clock f = 
  let t0 = Unix.gettimeofday () in
  let () = f() in
  let t1 = Unix.gettimeofday () in
  t1 -. t0
module MyLog = (Flog0:Log.LOG) (* HERE *)

module MyDB = DB(MyLog)

let make_key i = Printf.sprintf "key_%08i" i 

let set_loop db vs n = 
  let v = String.make vs 'x' in
  let set k v = MyDB.set db k v in
  let rec loop i = 
    if i = n 
    then MyLog.sync db
    else
      let key = make_key i in
      let () = set key v in
      loop (i+1)
  in
  loop 0

let get_loop db n = 
  let get k = MyDB.get db k in
  let rec loop i =
    if i = n 
    then ()
    else
      let key = make_key i in
      let _ = get key in
      loop (i+1)
  in
  loop 0

let delete_loop db n = 
  let delete k = MyDB.delete db k in
  let rec loop i = 
    if i = n 
    then MyLog.sync db
    else
      let key = make_key i in
      let () = delete key in
      loop (i+1)
  in
  loop 0

let () = 
  let n  = ref 1_000_000 in
  let vs = ref 2_000 in
  let fn = ref "test.db" in
  let d = ref 4 in
  let dump = ref false in
  let () = 
    Arg.parse [
      ("--value-size",Set_int vs, Printf.sprintf "size of the values in bytes (%i)" !vs);
      ("--file", Set_string fn, Printf.sprintf "file name for database (%S)" !fn);
      ("--bench-size",Set_int n,  Printf.sprintf "number of sets/gets/deletes (%i)" !n);
      ("--d", Set_int d, Printf.sprintf "1/2 of the fan-out (%i)" !d);
      ("--dump", Set dump, Printf.sprintf "doesn't run a benchmark, but dumps info about the file");
    ]
      (fun _ ->()) 
      "simple baardskeerder benchmark"
  in
  if !dump then
    begin
      let db = MyLog.make !fn in
      MyLog.dump db;
      MyLog.close db
    end
  else
    begin
      let () = MyLog.init !fn ~d:!d in 
      let db = MyLog.make !fn in
      let () = Printf.printf "\niterations = %i\nvalue_size = %i\n%!" !n !vs in
      let d = clock (fun () -> set_loop db !vs !n) in
      Printf.printf "%i sets: %fs\n%!" !n d;
      let d2 = clock (fun () -> get_loop db !n) in
      Printf.printf "%i gets: %fs\n%!" !n d2;
      let d3 = clock (fun () -> delete_loop db !n) in
      Printf.printf "%i deletes: %fs\n%!" !n d3;
      let () = MyLog.close db in
      ()
    end;;
