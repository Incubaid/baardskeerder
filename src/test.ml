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
open Base

module MDB = DB(Mlog)

type 'a q = 'a * ('a -> k -> v -> unit) * ('a -> k -> v) * ('a -> k -> unit)

let mem_setup () =  (Mlog.make 40 , MDB.set, MDB.get, MDB.delete)

let mem_teardown q = ()

let mem_wrap t = OUnit.bracket mem_setup t mem_teardown

let check (log,_,get,_) kvs = 
  List.iter (fun (k,v) -> OUnit.assert_equal v (get log k)) kvs 

let check_not (log,_,get,_) kvs = 
  List.iter (fun (k,_) -> OUnit.assert_raises Not_found (fun () -> get log k)) kvs

let set_all (log,set,_,_) kvs = List.iter (fun (k,v) -> set log k v) kvs


let delete_all_check ((log,set,get,delete) as q) kvs = 
  let rec loop acc = function
    | [] -> ()
    | (k,v) as h :: t -> 
      delete log k;
      let acc' = h :: acc in
      check_not q acc';
      check q t;
      loop acc' t
  in loop [] kvs
      
let insert_delete_generic q kvs = 
  set_all q kvs;
  check q kvs;
  delete_all_check q (List.rev kvs)


let insert_delete_1 ((log,set,get,delete) as q) = 
  set log "a" "A";
  check q ["a","A"];
  delete log "a";
  check_not q ["a","A"]

let kvs = ["a","A";"d","D";"g","G";"j","J";"m","M";"q","Q";"t","T";"w","W"]

let take n  = 
  let rec fill acc es = function
    | 0 -> List.rev acc
    | n -> match es with h::t -> fill (h::acc) t (n-1)
  in
  fill [] kvs n

let insert_delete_2 q = insert_delete_generic q (take 2)

let insert_delete_3 q = insert_delete_generic q (take 3)

let insert_delete_4 q = insert_delete_generic q (take 4)

let insert_delete_5 q = insert_delete_generic q (take 5)

let insert_delete_6 q = insert_delete_generic q (take 6)
  
let insert_delete_7 q = insert_delete_generic q (take 7)

let insert_delete_8 q = insert_delete_generic q (take 8)

let split_1 ((log,set,get,delete) as q) =   
  let kvs0 = ["a","A"; "d","D"; "g","G";] in
  set_all q kvs0;
  set log "j" "J";
  check q (("j","J")::kvs0);
  delete log "j";
  check q kvs0

  
let split_2 ((log,set,get,delete) as q) = 
  let kvs0 = ["a","A"; "d","D"; "g","G"; "j","J"; "m","M";] in
  set_all q kvs0;
  set log "q" "Q";
  check q (("q","Q")::kvs0);
  delete log "q";
  check q kvs0



let suite = 
  "correctness" >::: [
    "insert_delete_1" >:: mem_wrap insert_delete_1;
    "insert_delete_2" >:: mem_wrap insert_delete_2;
    "insert_delete_3" >:: mem_wrap insert_delete_3;
    "insert_delete_4" >:: mem_wrap insert_delete_4;
    "split_1" >:: mem_wrap split_1;
    "split_2" >:: mem_wrap split_2;
    "insert_delete_5" >:: mem_wrap insert_delete_5;
    "insert_delete_6" >:: mem_wrap insert_delete_6;
    "insert_delete_7" >:: mem_wrap insert_delete_7;
    "insert_delete_8" >:: mem_wrap insert_delete_8;
  ]


let _ = 
  run_test_tt_main suite;;
