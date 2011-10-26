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
let kvs_x = 
  let rec loop acc = function
    | 0 -> acc
    | i -> let key = Printf.sprintf "key_%02i" i in
	   let value = Printf.sprintf "V%i" i in
	   let acc' = (key,value)::acc in
	   loop acc' (i-1) 
  in
  loop [] 30

(*
 0: Value "A"
 2: Value "B"
 4: Value "C"
 6: Value "D"
10: Value "E"
13: Value "G"
17: Value "J"
20: Value "M"
26: Value "Q"
30: Value "T"
35: Value "W"
39: Value "Z"

// delete "a" has problems

*)

let kvs = ["a";"b"; "c"; "d";"e"; 
	   "g";"j"; "m"; "q"; "t";
	   "w";"z"]

let () = List.iter 
  (fun k -> 
    let v = String.uppercase k in
    let () = MDB.set t0 k v in
    (* let _ = MDot.view_tree t0 in  *)
    ()
  ) kvs;;
(*
let check () = List.iter (fun (k,v) -> assert (MDB.get t0 k =v)) kvs;;
let () = check ();;*)
(* MDB.set t0 "m" "M";; *)
MDot.view_tree t0;;
MDB.delete t0 "a";;
MDot.view_tree t0;;
(* MDB.delete t0 "t";; *)

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
