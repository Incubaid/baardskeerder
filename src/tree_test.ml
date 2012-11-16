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
open QuickCheck

open Tree
open Base
open Index
open Leaf
open Entry

module MDB = DB(Mlog)
open Test_helper

type 'a q = {
  log:'a; 
  last:  'a  -> pos;
  read:  'a  -> pos -> entry;
  clear: 'a  -> unit;
  set:   'a  -> k -> v -> unit;
  get:   'a  -> k -> (v,k) result;
  delete :'a -> k -> (unit,k) result;
  dump   :?out:out_channel -> 'a -> unit;
}

let _ok_delete q a =
  let r = q.delete q.log a in
  assert (r = OK ());
  ()

let mem_setup () =  
  let fut = Time.make 1L 2 false in
  let empty = Slab.make fut in
  {
  log = Mlog.make "bla";
  last = Mlog.last;
  read = Mlog.read;
  clear = Mlog.clear;
  set = MDB.set;
  get = (fun log k -> MDB._get log empty k);
  delete = MDB.delete;
  dump = Mlog.dump;
}

let mem_teardown _ = ()

let mem_wrap t = OUnit.bracket mem_setup t mem_teardown



let check q kvs = 
  List.iter (fun k ->
    let v = String.uppercase k in
    let vo = OK v in
    OUnit.assert_equal vo (q.get q.log k)) kvs 

let check_empty q =
  let i = q.last q.log in
  let n = q.read q.log i in
  match n with
    | Commit c -> 
      let pos = Commit.get_pos c in
      let e = q.read q.log pos in
      OUnit.assert_equal e (Leaf [])
    | _ -> failwith "last is not a commit entry"


let check_invariants (q: 'a q) = 
  let rec max_key i = 
    let n = q.read q.log i in
    match n with
      | NIL           -> failwith "corrupt"
      | Value _       -> failwith "corrupt"
      | Leaf leaf     -> leaf_max_key leaf
      | Index (p0,kps) ->
	let p = 
	  let rec loop = function
	    | []    -> p0
	    | [_,p] -> p
	    | _ :: t -> loop t
	  in
	  loop kps
	in
	max_key p
  in	       
  let rec walk i = 
    let n = q.read q.log i in
    match n with 
      | NIL -> ()
      | Value _ -> failwith "corrupt"
      | Leaf _ -> ()
      | Index (p0,kps) ->
	let rec loop p = function
	  | [] -> walk p
	  | (ke,pr) :: t -> 
	    walk p;
	    let k = max_key p in
	    
	    OUnit.assert_equal ~printer:(fun s -> s) ~msg:"separator does not match lower-right" ke k;
	    loop pr t
	in
	loop p0 kps
  in
  let i = q.last q.log in
  let n = q.read q.log i in
  match n with
    | Commit c -> let pos = Commit.get_pos c in walk pos
    | NIL -> () 
    | e -> let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in 
	   failwith s
									
let check_not q kvs = 
  List.iter (fun k -> OUnit.assert_equal (NOK k) (q.get q.log k)) kvs 
    
    

let insert_delete_1 q = 
  q.set q.log "a" "A";
  check q ["a"];
  _ok_delete q "a";
  check_not q ["a"]



      
let set_all q kvs = List.iter (fun k -> let v = String.uppercase k in q.set q.log k v) kvs

let delete_all_check q kvs = 
  let rec loop acc = function
    | [] -> ()
    | k :: t -> 
      _ok_delete q k;
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
    | n -> match es with 
	| [] -> failwith "empty" 
	| h::t -> fill (h::acc) t (n-1)
  in
  fill [] kvs n

let insert_delete_2 (q:'a q) = insert_delete_generic q (take 2)

let insert_delete_3 q = insert_delete_generic q (take 3)

let insert_delete_4 q = insert_delete_generic q (take 4)

let insert_delete_5 q = insert_delete_generic q (take 5)

let insert_delete_6 q = insert_delete_generic q (take 6)
  
let insert_delete_7 q = insert_delete_generic q (take 7)

let insert_delete_8 q = insert_delete_generic q (take 8)


let insert_delete_bug q = 
  let kvs =  
    ["a";"b"; "j"; "d";
     "g"; "m"; "q"; "t";
     "w";"z"]
  in
  List.iter (fun k -> let v = String.uppercase k in q.set q.log k v) kvs;
  _ok_delete q "a";
  _ok_delete q "b";
  _ok_delete q "j";
  ()
  
let insert_delete_bug2 q =
  let kvs = ["a";"b"; "c"; "d";"e"; 
	   "g";"j"; "m"; "q"; "t";
	   "w";"z"]
  in
  List.iter (fun k -> let v = String.uppercase k in q.set q.log k v) kvs;
  _ok_delete q "a";
  let kvs' = List.filter ( (<>) "a") kvs in
  List.iter (fun k -> let vo = OK (String.uppercase k) in 
		      let vo2 = q.get q.log k in
		      OUnit.assert_equal vo vo2) kvs'

let insert_delete_bug3 q = 
  let kvs = ["a";"b";"c";"d";"e";
	     "f";"g";"h";"i";"j";
	     "k";"l";"m";"n";"o";
	     "p";]
  in
  List.iter (fun k -> let v = String.uppercase k in q.set q.log k v) kvs;
  set_all q kvs;
  check q kvs;
  _ok_delete q "a";
  ()

let split_1 q =   
  let kvs0 = ["a"; "d"; "g"] in
  set_all q kvs0;
  q.set q.log "j" "J";
  check q ("j"::kvs0);
  _ok_delete q "j";
  check q kvs0

  
let split_2 q = 
  let kvs0 = ["a"; "d"; "g"; "j"; "m";] in
  set_all q kvs0;
  q.set q.log "q" "Q";
  check q ("q"::kvs0);
  _ok_delete q "q";
  check q kvs0

let underflow_n2 q = 
  let kvs = ["a"; "d"; "g"; "j"; "m";"q"] in
  set_all q kvs;
  _ok_delete q "g"

let underflow_n2_2 q = 
  let kvs = ["a";"d"; "g"; "j";"m"; "q"] in
  set_all q kvs;
  _ok_delete q "j"

let insert_overflow q = 
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

let insert_delete_permutations_generic  n q =
  let kvs = take n in 
  let kvs' = Array.of_list kvs in
  Array.fast_sort String.compare kvs';

  let l = Array.length kvs' in

  let do_test n a =
    try
      q.clear q.log;
      if n mod 500 = 0 then Printf.printf "n=%i\n%!" n;
      Array.iter (fun k -> q.set q.log k (String.uppercase k)) a;
      check q (Array.to_list a);
      Array.iter (fun k -> let () = check_invariants q in _ok_delete q k) a;
      check_empty q
    with 
      | e -> 	
	Printf.fprintf stderr "Sequence: %s\n" (Pretty.string_of_list 
						  (fun s -> Printf.sprintf "%S" s) (Array.to_list  a));
	raise e
  in

  let rec loop = function
    | 0 -> ()
    | n ->
        do_test n kvs';
        if n > 1 then next_permutation kvs' else ();
        loop (pred n)
  in
  loop (fac l)

let debug_info_wrap f = fun q ->
  try 
    f q 
  with 
      e ->
	begin
	  Printf.fprintf stderr "Exception: %s\n" (Printexc.to_string e);
	  Printf.fprintf stderr "%s\n" (Printexc.get_backtrace ());
	  Printf.fprintf stderr "Tree:\n%!";
	  q.dump ~out:stderr q.log;
	  flush stderr;
	  raise e
	end



let insert_static_delete_permutations_generic  n (q: 'a q) =
  let kvs = take n in 
  let kvs' = Array.of_list kvs in
  Array.fast_sort String.compare kvs';

  let l = Array.length kvs' in

  let do_test n a =
    try
      q.clear q.log;
    (*Printf.printf "-----\n"; *)
      if n mod 500 = 0 then Printf.printf "n=%i\n%!" n;
      List.iter (fun k -> q.set q.log k (String.uppercase k)) kvs;
      check q kvs;
      Array.iter (fun k -> check_invariants q; _ok_delete q k) a;
      check_empty q
    with
      e -> 
	Printf.fprintf stderr "Sequence: %s\n" (Pretty.string_of_list (fun s -> s) (Array.to_list  a));
	raise e
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

let _insert_delete_bugx max q =
  let rec loop1 = function
    | 0 -> ()
    | n ->
      let k = Printf.sprintf "key_%d" n
      and v = Printf.sprintf "value_%d" n in
      Printf.printf "Set %s\n%!" k;      
      q.set q.log k v;
      loop1 (pred n)
  in
  loop1 max;
  let rec loop2 = function
    | 0 -> ()
    | n ->
      check_invariants q;
      let k = Printf.sprintf "key_%d" n in
      Printf.printf "Delete %s\n%!" k;
      _ok_delete q k;
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
    (*"insert_delete_permutations_6", debug_info_wrap (insert_delete_permutations_generic 12);*)
    (* 11 :  834.8 s *)
    (* 12 :  9600 s  *)
    (* 13 :  *)
    "insert_static_delete_permutations_1", debug_info_wrap (all_n 5);
    "insert_static_delete_permutations_2", debug_info_wrap (all_n 6); 
    "insert_static_delete_permutations_3", debug_info_wrap (all_n 7); 
    "insert_static_delete_permutations_4", debug_info_wrap (all_n 8);
    "insert_static_delete_permutations_5", debug_info_wrap (all_n 9);
    "insert_delete_bug4", _insert_delete_bugx 100;
    "insert_delete_bug5", _insert_delete_bugx 20;
    "insert_delete_bug6", _insert_delete_bugx 104;
    "insert_delete_bug7", _insert_delete_bugx 109;
    "insert_delete_bug8", _insert_delete_bugx 156;
  ]

let make_suite wrap = (List.map (fun (n,t) -> n >:: wrap t) template)
let unit_suite = "Unit" >::: make_suite mem_wrap


(* QuickCheck property tests *)

let qc_insert_lookup log = fun kvs ->
  List.iter (fun (k, v) -> MDB.set log k v) kvs;
  let (r, _) = List.fold_right
    (fun (k, v) (a, ks) ->
      if List.mem k ks
      then
        (a, ks)
      else
	    let vo = OK v in
        let vo' = MDB.get log k in
        (a && (vo' = vo), k :: ks)
    )
    kvs (true, [])
  in
  r

let qc_insert_delete log = fun kvs ->
  List.iter (fun (k, v) -> MDB.set log k v) kvs;
  let _ = List.fold_right
    (fun (k, _) ks -> 
      if List.mem k ks 
      then ks 
      else let r = MDB.delete log k in 
           assert (r = OK ()); 
           k :: ks
    )
    kvs []
  in
  MDB.range log None true None true None = []

let qc_replace log = fun (k, vs) ->
  List.iter (fun v -> MDB.set log k v) vs;
  match vs with
    | [] -> begin
      let vo = MDB.get log k in
      match vo with
	| OK _  -> false
	| NOK _ -> true
      end
    | _ -> MDB.get log k = OK (List.nth vs (List.length vs - 1))

let qc_key_value_list =
  let ak = arbitrary_string
  and sk = show_string
  and av = arbitrary_string
  and sv = show_string in
  let akv = arbitrary_pair ak av
  and skv = show_pair sk sv in
  let akvl = arbitrary_list akv
  and skvl = show_list skv in

  testable_fun akvl skvl testable_bool

let qc_key_value_list_tuple =
  testable_fun
    (arbitrary_pair arbitrary_string (arbitrary_list arbitrary_string))
    (show_pair show_string (show_list show_string))
    testable_bool

let qc_wrap r t () =
  match (quickCheck r) (fun a -> t (Mlog.make "mlog") a) with
    | Success -> ()
    | Failure n ->
        assert_failure (Printf.sprintf "QuickCheck failed after %d tests" n)
    | Exhausted n ->
        skip_if true (Printf.sprintf "QuickCheck exhausted after %d runs" n)

let qc_suite = "QuickCheck" >::: [
  "insert_lookup" >:: qc_wrap qc_key_value_list qc_insert_lookup;
  "insert_delete" >:: qc_wrap qc_key_value_list qc_insert_delete;
  "replace" >:: qc_wrap qc_key_value_list_tuple qc_replace;
]


let suite = "Tree" >::: [
  unit_suite;
  qc_suite;
]
