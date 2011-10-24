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

open Dot
open Tree
open Entry
open Mlog

module MDB = DB(Mlog);;

module MDot = Dot(Mlog);; 

let t0 = Mlog.make ();;
let kvs =  [
  "a", "A";
  "d", "D";
  "g", "G"; 
  "j", "J";
  "m", "M";
  "q", "Q";
  "t", "T";
  "w", "W"; 
  
(*  
    "z", "Z"; 
    "z1","Z1";
    "z2","Z2";
    "z3","Z3";
    "z4","z4";
    "z5","z5";
    "z6","Z6";
    "z7","Z7";
    "z8","Z8";
    "z9","Z9";
    "z10","Z10"; 
*)

];;

let () = List.iter 
  (fun (k,v) -> 
    let () = MDB.set t0 k v in
    ()
  ) kvs;;
let check () = List.iter (fun (k,v) -> assert (MDB.get t0 k =v)) kvs;;
let () = check ();;

MDot.view_tree t0;;
MDB.delete t0 "a";;

(* MDB.delete t0 "w";;
   MDot.view_tree t0;; *)
(* let _  = MDot.view_tree t0;; *)
(* let () = MDB.delete t0 "q";; *)


(*
let () = 
  let t1 = Mlog.make 40 in
  let kvs = ["z","Z";
	     "w","W"; 
	     "t","T";  
	     "q","Q"; 
	    ] ;
  in
  List.iter (fun (k,v) -> MDB.set t1 k v ) kvs;
  Mlog.dump t1;
  let _ = MDot.view_tree t1 in
  MDB.set t1 "m" "M";
  let _ = MDot.view_tree t1 in 
  ();;
*)
