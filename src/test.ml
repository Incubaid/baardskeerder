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

open OUnit
open Log
open Tree
open Entry
module MDB = DB(Mlog)

type 'a q = 'a * ('a -> k -> v -> unit) * ('a -> k -> v) * ('a -> k -> unit)

let mem_setup () =  (Mlog.make 40 , MDB.set, MDB.get, MDB.delete)

let mem_teardown q = ()

let mem_wrap t = OUnit.bracket mem_setup t mem_teardown

let check (log,_,get,_) kvs = 
  List.iter (fun (k,v) -> OUnit.assert_equal v (get log k)) kvs 

let split_1 ((log,set,get,delete) as q) =   
  let kvs0 = ["a","A"; "d","D"; "g","G";] in
  List.iter (fun (k,v) -> set log k v) kvs0;
  set log "j" "J";
  check q (("j","J")::kvs0);
  delete log "j";
  check q kvs0


let split_2 ((log,set,get,delete) as q) = 
  let kvs0 = ["a","A"; "d","D"; "g","G"; "j","J"; "m","M";] in
  List.iter (fun (k,v) -> set log k v) kvs0;
  set log "q" "Q";
  check q (("q","Q")::kvs0);
  delete log "q";
  check q kvs0

let suite = 
  "correctness" >::: [
    "split_1" >:: mem_wrap split_1;
    "split_2" >:: mem_wrap split_2;
  ]


let _ = 
  run_test_tt_main suite;;
