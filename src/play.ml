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
  let i = Mlog.root t in
  walk i;;

let test max t0 =
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
      if List.mem n  [13;12] 
      then 
	let _ = MDot.view t0 in 
	(* let () = Mlog.dump t0 in *)
	()
      else ();
      check_invariants t0; 
      Printf.printf "going to delete %s\n%!" k;
      MDB.delete t0 k;
      Printf.printf "deleted %s\n%!" k;
      loop2 (pred n)
  in
  loop2 max;;

let find_problem () =
  let rec loop x = 
    if x = 100 then () 
    else
      let () = test x t0 in
      let () = Mlog.clear t0 in
      loop (x + 1)
  in
  loop 1;;

test 15 t0;; 
(* find_problem () *)

  
(*

*)

(*
List.iter (fun k -> MDB.set t0 k (String.uppercase k)) ["a"; "b"; "c"; "j"; "m"; "q"; "d"; "t"; "w"; "g"; "z"];;
MDot.view_tree t0;;
MDB.delete t0 "a";;
MDot.view_tree t0;;
*)
(*
  MDot.view_tree t0;;
  MDB.delete t0 "b";;
*)
(*
  MDot.view_tree t0;;
  MDB.delete t0 "d";;
  MDot.view_tree t0;;
*)

(*
let () = List.iter 
  (fun k -> 
    let v = String.uppercase k in
    let () = MDB.set t0 k v in
    let _ = MDot.view_tree t0 in 
    ()
  ) kvs;;
*)
(*
let check () = List.iter (fun (k,v) -> assert (MDB.get t0 k =v)) kvs;;
let () = check ();;*)
(* MDB.set t0 "m" "M";; *)

(*
MDB.delete t0 "a";;
MDot.view_tree t0;;
*)
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
