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

open Base
open Dot
open Tree
open Entry
open Mlog

module MDB = DB(Mlog);;
module MDot = Dot(Mlog);; 



let t0 = Mlog.make "xxx";;
(*
let kvs_x = 
  let rec loop acc = function
    | 0 -> acc
    | i -> let key = Printf.sprintf "key_%02i" i in
	   let value = Printf.sprintf "V%i" i in
	   let acc' = (key,value)::acc in
	   loop acc' (i-1) 
  in
  loop [] 30

let kvs =
  ["a";"b";"c";"d";"e";
   "f";"g";"h";"i";"j";
   "k";"l";"m";"n";"o";];;
   (* "p";] *)

let check_invariants t = 
  let rec max_key i = 
    let n = Mlog.read t i in
    match n with
      | NIL           -> failwith "corrupt"
      | Value v       -> failwith "corrupt"
      | Leaf leaf     -> Leaf.leaf_max_key leaf
      | Index (p0,kps) ->
	let p = 
	  let rec loop = function
	    | []    -> p0
	    | [_,p] -> p
	    | h :: t -> loop t
	  in
	  loop kps
	in
	max_key p
  in	       
  let rec walk i = 
    let n = Mlog.read t i in
    match n with
      | Commit p -> walk p
      | NIL -> ()
      | Leaf leaf -> () (* Printf.printf "leaf=%s\n" (Leaf.leaf2s leaf) *)
      | Index index ->
	let p0, kps = index in
	let rec loop p = function
	  | [] -> walk p
	  | (ke,pr) :: t -> 
	    walk p;
	    let k = max_key p in
	    (* let () = Printf.printf "separator: %s =?= %s\n" ke k in *)
	    if k <> ke then
	      let s = Printf.sprintf "separator %s does not match lower_right %s" ke k in
	      failwith s
	    else
	      loop pr t
	in
	loop p0 kps
  in
  let i = Mlog.last t in
  walk i;;

let test max t0 =
  Printf.printf "test %i\n%!" max;
  let rec loop1 = function
    | 0 -> ()
    | n ->
      let k = Printf.sprintf "key_%d" n
      and v = Printf.sprintf "value_%d" n in
      Printf.printf "Set %s\n%!" k; 
      MDB.set t0 k v;
      loop1 (pred n)
  in
  loop1 max;
  (* let _ = MDot.view_tree t0 in (); *)
  (*let () = MDB.delete t0 "key_20" in
  let _ = MDot.view_tree t0 in () *)
  let rec loop2 = function
    | 0 -> ()
    | n ->
      let k = Printf.sprintf "key_%d" n in
      if false (*List.mem n  [14] *)
      then 
	let _ = MDot.view t0 in 
	(* let () = Mlog.dump t0 in *)
	()
      else ();
      Printf.printf "going to delete %s\n%!" k;
      MDB.delete t0 k;
      Printf.printf "deleted %s\n%!" k;
      check_invariants t0; 
      loop2 (pred n)
  in
  loop2 max;;

let find_problem () =
  let rec loop x = 
    if x = 1001 then () 
    else
      let () = test x t0 in
      let () = Mlog.clear t0 in
      loop (x + 1)
  in
  loop 1;;

test 109 t0;;

*)
MDB.set t0 "a" "A";;
MDB.set t0 "b" "B";;
MDB.set t0 "c" "C";;
