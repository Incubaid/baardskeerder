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

type 'a q = 'a * ('a -> k -> v -> unit) * ('a -> k -> v) * ('a -> k -> unit)

let mem_setup () =  (Mlog.make (), MDB.set, MDB.get, MDB.delete)

let mem_teardown q = ()

let mem_wrap t = OUnit.bracket mem_setup t mem_teardown



let check (log,_,get,_) kvs = 
  List.iter (fun (k,v) -> 
    OUnit.assert_equal v (get log k)) kvs 

let check_empty (log,_,get,_) =
  let i = Mlog.root log in
  let n = Mlog.read log i in
  OUnit.assert_equal n (Leaf [])


    
let check_not (log,_,get,_) kvs = 
  List.iter (fun (k,_) -> 
    OUnit.assert_raises ~msg:k (NOT_FOUND k) (fun () -> get log k)) kvs    
    
    

let insert_delete_1 ((log,set,get,delete) as q) = 
  set log "a" "A";
  check q ["a","A"];
  delete log "a";
  check_not q ["a","A"]


let t_neigbours () = 
  let z = Loc ((7, [("j", 15); ("d", 14)]), []) in
  let nb = Index.indexz_neighbours z in
  OUnit.assert_equal (NL 14) nb

let t_suppress () = 
  let z = Loc ((7, [("j", 15); ("d", 14)]), []) in
  let nb = Index.indexz_neighbours z in
  match nb with 
    | NL 14 ->
      let index = Index.indexz_suppress L 17 z in
      Printf.printf "index = %s\n" (index2s index)
    | _ -> failwith "should be NL 14"

let t_suppress2 () = 
  let z = Loc ((7,["d", 8]),[]) in
  let nb = Index.indexz_neighbours z in
  match nb with 
    | NL 7 ->
      let index = Index.indexz_suppress L 17 z in
      Printf.printf "index = %s\n" (index2s index)
    | _ -> failwith "should be NL 7"

let t_balance() =
  let z = Loc ((7,["q", 22; "j", 21; "d", 14]),[]) in
  let z' = Index.indexz_balance z in
  match z' with
    | Loc ((_,l),r) -> let ls = List.length l in
		       let rs = List.length r in
		       OUnit.assert_equal (ls+1) rs
    | _ -> failwith "should be Loc"

let t_split () = 
  let lpos = 21
  and sep = "q"
  and rpos = 22
  and z = Loc ((7, [("j", 18); ("d", 14)]), [])
  in
  let left,sep', right = indexz_split lpos sep rpos z in
  let () = Printf.printf "left = %s\n" (index2s left) in
  let () = Printf.printf "right =%s\n" (index2s right) in
  OUnit.assert_equal (7, ["d",14]) left


let set_all (log,set,_,_) kvs = List.iter (fun (k,v) -> 
  Printf.printf "set %S %S\n%!" k v;
  set log k v) kvs

let delete_all_check ((log,set,get,delete) as q) kvs = 
  let rec loop acc = function
    | [] -> ()
    | (k,v) as h :: t -> 
      Printf.printf "delete %S\n%!" k;
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

let underflow_n2 ((log,_,_, delete) as q) = 
  let kvs = ["a","A"; "d","D"; "g","G"; "j","J"; "m","M";"q","Q"] in
  set_all q kvs;
  delete log "g"

let underflow_n2_2 ((log,_,_,delete) as q) = 
  let kvs = ["a","A"; "d","D"; "g","G"; "j","J"; "m","M";"q","Q"] in
  set_all q kvs;
  delete log "j"

  
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

let insert_delete_permutations_1 ((log,set,get,delete) as q) =
  let kvs = take 5 in (* TODO *)
  let kvs' = Array.of_list kvs in
  Array.fast_sort (fun (k1, _) (k2, _) -> compare k1 k2) kvs';

  let l = Array.length kvs' in

  let fst (a, _) = a in

  let do_test a =
    Array.iter (fun (k, v) -> set log k v) a;
    check q (Array.to_list a);
    Array.iter (fun (k, v) -> delete log k) a;
    check_empty q
  in

  let rec loop = function
    | 0 -> ()
    | n ->
        do_test kvs';
        if n > 1 then next_permutation kvs' else ();
        loop (pred n)
  in
  loop (fac l)

let debug_info_wrap f = fun ((log, _, _, _) as q) ->
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



let insert_static_delete_permutations_generic  n ((log, set, get, delete) as q) =
  let kvs = take n in 
  let kvs' = Array.of_list kvs in
  Array.fast_sort (fun (k1, _) (k2, _) -> compare k1 k2) kvs';

  let l = Array.length kvs' in

  let do_test n a =
    if n mod 500 = 0 then Printf.printf "n=%i\n%!" n;
    List.iter (fun (k, v) -> set log k v) kvs;
    check q kvs;
    Array.iter (fun (k, v) -> delete log k) a;
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


let tests = [
    "index_neighbours" >:: t_neigbours;
    "index_suppress" >:: t_suppress;
    "index_suppress2" >:: t_suppress2;
    "index_balance"    >:: t_balance;
    "index_split"     >:: t_split;
    "insert_delete_1" >:: mem_wrap insert_delete_1;
    "insert_delete_2" >:: mem_wrap insert_delete_2;
    "insert_delete_3" >:: mem_wrap insert_delete_3;
    "insert_delete_4" >:: mem_wrap insert_delete_4;
    "split_1" >:: mem_wrap split_1;
    "split_2" >:: mem_wrap split_2;
    "underflow_n2" >:: mem_wrap underflow_n2;
    "underflow_n2_2" >:: mem_wrap underflow_n2_2;
    "insert_delete_5" >:: mem_wrap insert_delete_5;
    "insert_delete_6" >:: mem_wrap insert_delete_6;
    "insert_delete_7" >:: mem_wrap insert_delete_7;
    "insert_delete_8" >:: mem_wrap insert_delete_8;
    "insert_delete_permutations_1" >::
      mem_wrap (debug_info_wrap insert_delete_permutations_1);
    "insert_static_delete_permutations_1" >:: mem_wrap (debug_info_wrap (all_n 5));
    "insert_static_delete_permutations_2" >:: mem_wrap (debug_info_wrap (all_n 6)); 
    "insert_static_delete_permutations_3" >:: mem_wrap (debug_info_wrap (all_n 7)); 
    "insert_static_delete_permutations_4" >:: mem_wrap (debug_info_wrap (all_n 8)); (* takes about 500 s *)
    
]
