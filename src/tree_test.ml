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
open Tree
open Base
open Index
open Leaf
open Entry

module MDB = DB(Mlog)

type 'a q = 'a * ('a -> k -> v -> unit) * ('a -> k -> v) * ('a -> k -> unit) * (unit -> 'a)

let mem_setup () =  (Mlog.make (), MDB.set, MDB.get, MDB.delete, Mlog.clear)

let mem_teardown q = ()

let mem_wrap t = OUnit.bracket mem_setup t mem_teardown



let check ((log,_,get,_,_)) kvs = 
  List.iter (fun k ->
    let v = String.uppercase k in
    OUnit.assert_equal v (get log k)) kvs 

let check_empty (log,_,get,_,_) =
  let i = Mlog.root log in
  let n = Mlog.read log i in
  OUnit.assert_equal n (Leaf [])


    
let check_not (log,_,get,_,_) kvs = 
  List.iter (fun k -> 
    OUnit.assert_raises ~msg:k (NOT_FOUND k) (fun () -> get log k)) kvs    
    
    

let insert_delete_1 ((log,set,get,delete,_) as q) = 
  set log "a" "A";
  check q ["a"];
  delete log "a";
  check_not q ["a"]



      
let set_all (log,set,_,_,_) kvs = List.iter (fun k -> let v = String.uppercase k in set log k v) kvs

let delete_all_check ((log,set,get,delete,_) as q) kvs = 
  let rec loop acc = function
    | [] -> ()
    | k :: t -> 
      delete log k;
      let acc' = k :: acc in
      check_not q acc';
      check q t;
      loop acc' t
  in loop [] kvs


let insert_delete_generic q kvs = 
  set_all q kvs;
  check q kvs;
  delete_all_check q (List.rev kvs)




let kvs = ["a";"d";"g";"j";"m";
	   "q";"t";"w";"z";"b";
	   "c";"e";"f";"h";"i";
	   "k";"l";
	  ]

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


let insert_delete_bug ((log, set, get, delete,_) as q) = 
  let kvs =  
    ["a";"b"; "j"; "d";
     "g"; "m"; "q"; "t";
     "w";"z"]
  in
  List.iter (fun k -> let v = String.uppercase k in set log k v) kvs;
  delete log "a";
  delete log "b";
  delete log "j"
  
let insert_delete_bug2 ((log,set,get,delete,_) as q) =
  let kvs = ["a";"b"; "c"; "d";"e"; 
	   "g";"j"; "m"; "q"; "t";
	   "w";"z"]
  in
  List.iter (fun k -> let v = String.uppercase k in set log k v) kvs;
  delete log "a";
  let kvs' = List.filter ( (<>) "a") kvs in
  List.iter (fun k -> let v = String.uppercase k in 
		      let v2 = get log k in
		      OUnit.assert_equal v v2) kvs'

let insert_delete_bug3 ((log,set,get,delete,_) as q) = 
  let kvs = ["a";"b";"c";"d";"e";
	     "f";"g";"h";"i";"j";
	     "k";"l";"m";"n";"o";
	     "p";]
  in
  List.iter (fun k -> let v = String.uppercase k in set log k v) kvs;
  set_all q kvs;
  check q kvs;
  delete log "a";
  ()

let split_1 ((log,set,get,delete,_) as q) =   
  let kvs0 = ["a"; "d"; "g"] in
  set_all q kvs0;
  set log "j" "J";
  check q ("j"::kvs0);
  delete log "j";
  check q kvs0

  
let split_2 ((log,set,get,delete,_) as q) = 
  let kvs0 = ["a"; "d"; "g"; "j"; "m";] in
  set_all q kvs0;
  set log "q" "Q";
  check q ("q"::kvs0);
  delete log "q";
  check q kvs0

let underflow_n2 ((log,_,_, delete,_) as q) = 
  let kvs = ["a"; "d"; "g"; "j"; "m";"q"] in
  set_all q kvs;
  delete log "g"

let underflow_n2_2 ((log,_,_,delete,_) as q) = 
  let kvs = ["a";"d"; "g"; "j";"m"; "q"] in
  set_all q kvs;
  delete log "j"

let insert_overflow ((log,set,get,delete,_) as q) = 
  let kvs =  ["a"; "d"; "g"; "m";"q"; "t"; "j";] in
  set_all q kvs;
  check q kvs

let fac =
  let rec helper acc = function
    | 0 -> acc
    | n -> helper (acc * n) (pred n)
  in
  helper 1

let next_permutation a =
  let swap a i j =
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  in

  let i = ref (Array.length a - 1) in

  while a.(!i - 1) >= a.(!i) do
      decr i
  done;

  let j = ref (Array.length a) in
  while a.(!j - 1) <= a.(!i - 1) do
      decr j
  done;

  swap a (!i - 1) (!j - 1);

  incr i;
  j := Array.length a;

  while !i < !j do
      swap a (!i - 1) (!j - 1);
      incr i;
      decr j;
  done

let insert_delete_permutations_generic  n ((log,set,get,delete, clear) as q) =
  let kvs = take n in 
  let kvs' = Array.of_list kvs in
  Array.fast_sort String.compare kvs';

  let l = Array.length kvs' in

  let fst (a, _) = a in

  let do_test n a =
    clear log;
    if n mod 500 = 0 then Printf.printf "n=%i\n%!" n;
    Array.iter (fun k -> set log k (String.uppercase k)) a;
    check q (Array.to_list a);
    Array.iter (fun k -> (* Printf.eprintf "delete %s\n" k; *) delete log k) a;
    check_empty q
  in

  let rec loop = function
    | 0 -> ()
    | n ->
        do_test n kvs';
        if n > 1 then next_permutation kvs' else ();
        loop (pred n)
  in
  loop (fac l)

let debug_info_wrap f = fun ((log, _, _, _,_) as q) ->
  try 
    f q 
  with 
      e ->
	begin
	  Printf.fprintf stderr "Exception: %s\n" (Printexc.to_string e);
	  Printf.fprintf stderr "%s\n" (Printexc.get_backtrace ());
	  Printf.fprintf stderr "Tree:\n%!";
	  Mlog.dump ~out:stderr log;
	  flush stderr;
	  raise e
	end



let insert_static_delete_permutations_generic  n ((log, set, get, delete, clear) as q) =
  let kvs = take n in 
  let kvs' = Array.of_list kvs in
  Array.fast_sort String.compare kvs';

  let l = Array.length kvs' in

  let do_test n a =
    clear log;
    if n mod 500 = 0 then Printf.printf "n=%i\n%!" n;
    List.iter (fun k -> set log k (String.uppercase k)) kvs;
    check q kvs;
    Array.iter (fun k -> delete log k) a;
    check_empty q
  in

  let rec loop = function
    | 0 -> ()
    | n ->
        do_test n kvs';
        if n > 1 then next_permutation kvs' else ();
        loop (pred n)
  in
  loop (fac l)

let all_n n q = insert_static_delete_permutations_generic n q 

let _insert_delete_bugx max ((log,set,get,delete,_) as q) =
  let rec loop1 = function
    | 0 -> ()
    | n ->
      let k = Printf.sprintf "key_%d" n
      and v = Printf.sprintf "value_%d" n in
      Printf.fprintf (Pervasives.stderr) "Set %s\n" k;      
      set log k v;
      loop1 (pred n)
  in
  loop1 max;
  let rec loop2 = function
    | 0 -> ()
    | n ->
      let k = Printf.sprintf "key_%d" n in
      Printf.fprintf (Pervasives.stderr) "Delete %s\n" k;
      
      delete log k;
      loop2 (pred n)
  in
  loop2 max


let template =
  ["insert_delete_1",  insert_delete_1;
   "insert_delete_2",  insert_delete_2;
    "insert_delete_3", insert_delete_3;
    "insert_delete_4", insert_delete_4;
    "split_1", split_1;
    "split_2", split_2;
    "insert_overflow",insert_overflow;
    "underflow_n2", underflow_n2;
    "underflow_n2_2", underflow_n2_2;
    "insert_delete_5", insert_delete_5;
    "insert_delete_6", insert_delete_6;
    "insert_delete_7", insert_delete_7;
    "insert_delete_8", insert_delete_8;
    "insert_delete_bug", insert_delete_bug;
    "insert_delete_bug2", insert_delete_bug2;
    "insert_delete_bug3", insert_delete_bug3;
    "insert_delete_permutations_1", debug_info_wrap (insert_delete_permutations_generic 5);
    "insert_delete_permutations_2", debug_info_wrap (insert_delete_permutations_generic 6);
    "insert_delete_permutations_3", debug_info_wrap (insert_delete_permutations_generic 7);
    "insert_delete_permutations_4", debug_info_wrap (insert_delete_permutations_generic 8);
    "insert_delete_permutations_5", debug_info_wrap (insert_delete_permutations_generic 9);
    (* "insert_delete_permutations_6", debug_info_wrap (insert_delete_permutations_generic 17);  *)
    "insert_static_delete_permutations_1", debug_info_wrap (all_n 5);
    "insert_static_delete_permutations_2", debug_info_wrap (all_n 6); 
    "insert_static_delete_permutations_3", debug_info_wrap (all_n 7); 
    "insert_static_delete_permutations_4", debug_info_wrap (all_n 8);
    "insert_static_delete_permutations_5", debug_info_wrap (all_n 9);
    "insert_delete_bug4", _insert_delete_bugx 100;
    "insert_delete_bug5", _insert_delete_bugx 20;
  ]

let make_suite wrap = (List.map (fun (n,t) -> n >:: mem_wrap t) template)
let suite = "Tree" >::: make_suite mem_wrap

