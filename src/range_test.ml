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

open Tree
open OUnit
module MDB = DB(Mlog)

let printer r = Pretty.string_of_list (fun s -> s) r

let setup () = 
  let log = Mlog.make "mlog" in
  List.iter (fun k -> MDB.set log k (String.uppercase k)) ["a";"b";"c";"d";"e";"f";"g"];
  log
  
let teardown _ = ()

let range_all log = 
  let r = MDB.range log None true None true None in
  OUnit.assert_equal ~printer ["a";"b";"c";"d";"e";"f";"g"] r;;

let range_some log = 
    let r = MDB.range log None true None true (Some 5) in
  OUnit.assert_equal ~printer ["a";"b";"c";"d";"e"] r

let range_first log = 
  let r = MDB.range log (Some "b") true None true None in
  OUnit.assert_equal ~printer ["b";"c";"d";"e";"f";"g"] r

let range_first_exc log = 
  let r = MDB.range log (Some "b") false None true None in
  OUnit.assert_equal ~printer ["c";"d";"e";"f";"g"] r

let range_last log = 
  let r = MDB.range log None true (Some "d") true None in
  OUnit.assert_equal ~printer ["a";"b";"c";"d"] r

let range_last_exc log = 
  let r = MDB.range log None true (Some "d") false None in
  OUnit.assert_equal ~printer ["a";"b";"c";] r

let range_bounded_linc log =
  let r = MDB.range log (Some "c") true (Some "f") false None in
  OUnit.assert_equal ~printer ["c";"d";"e"] r

let range_bounded_finc log =
  let r = MDB.range log (Some "b") false (Some "f") true None in
  OUnit.assert_equal ~printer ["c";"d";"e";"f"] r

let range_bounded_linc_finc log =
  let r = MDB.range log (Some "b") true (Some "f") true None in
  OUnit.assert_equal ~printer ["b";"c";"d";"e";"f"] r

let range_bounded_overflow_left log =
  let r = MDB.range log (Some "0") true (Some "f") false None in
  OUnit.assert_equal ~printer ["a";"b";"c";"d";"e"] r

let range_bounded_overflow_right log =
  let r = MDB.range log (Some "c") false (Some "z") false None in
  OUnit.assert_equal ~printer ["d";"e";"f";"g"] r


(* Reverse range tests *)
let reverse_range_all log =
  let r = MDB.reverse_range log None true None true None in
  OUnit.assert_equal ~printer ["g";"f";"e";"d";"c";"b";"a"] r;;

let reverse_range_some log =
    let r = MDB.reverse_range log None true None true (Some 5) in
  OUnit.assert_equal ~printer ["g";"f";"e";"d";"c"] r

let reverse_range_first log =
  let r = MDB.reverse_range log (Some "f") true None true None in
  OUnit.assert_equal ~printer ["f";"e";"d";"c";"b";"a"] r

let reverse_range_first_exc log =
  let r = MDB.reverse_range log (Some "f") false None true None in
  OUnit.assert_equal ~printer ["e";"d"; "c";"b";"a"] r

let reverse_range_last log =
  let r = MDB.reverse_range log None true (Some "d") true None in
  OUnit.assert_equal ~printer ["g";"f";"e";"d"] r

let reverse_range_last_exc log =
  let r = MDB.reverse_range log None true (Some "d") false None in
  OUnit.assert_equal ~printer ["g";"f";"e";] r

let reverse_range_bounded_linc log =
  let r = MDB.reverse_range log (Some "f") true (Some "c") false None in
  OUnit.assert_equal ~printer ["f";"e";"d"] r

let reverse_range_bounded_finc log =
  let r = MDB.reverse_range log (Some "f") false (Some "b") true None in
  OUnit.assert_equal ~printer ["e";"d";"c";"b"] r

let reverse_range_bounded_linc_finc log =
  let r = MDB.reverse_range log (Some "f") true (Some "b") true None in
  OUnit.assert_equal ~printer ["f";"e";"d";"c";"b"] r

let reverse_range_bounded_overflow_left log =
  let r = MDB.reverse_range log (Some "z") true (Some "c") false None in
  OUnit.assert_equal ~printer ["g";"f";"e";"d"] r

let reverse_range_bounded_overflow_right log =
  let r = MDB.reverse_range log (Some "f") false (Some "0") false None in
  OUnit.assert_equal ~printer ["e";"d";"c";"b";"a"] r

let last ks = 
  let rec _loop = function
    | [] -> failwith "Empty"
    | [k] -> k
    | _::y -> _loop y
  in
  _loop ks

let fringe_lower log = 
  let boundary = Some "d" in
  let size = Some 3 in
  let limit = 100 in
  let (>>=) = Mlog.bind in
  let return = Mlog.return in
  MDB.set log "\xff" "omega" >>= fun () ->
  let rec loop start mem acc = 
    MDB.rev_range_entries log start false boundary false size >>= fun kvs_rev ->
    let len = List.length kvs_rev in
    if len = 0
    then return (List.rev acc, mem) 
    else
      let acc' = acc @ kvs_rev in
      let (lk,_) = last kvs_rev in
      let start' = Some lk in
      let mem' = List.fold_left 
        (fun mem (k,v) ->
          mem + String.length k + String.length v) mem kvs_rev 
      in
      if mem' < limit
      then loop start' mem' acc'
      else return (acc', mem')
  in
  let start = None (* Some "\xff\xff" *) in
  loop start 0 [] >>= fun x -> 
  let (ks,_) = x in
  print_newline();
  List.iter (fun (k,_) -> Printf.printf "k:%S\n" k ) ks;
  ()

let fringe_upper log = 
  let boundary = Some "e" in
  let size = Some 2 in
  let (>>=) = Mlog.bind in
  let return = Mlog.return in
  let rec loop start acc = 
    MDB.range log start false boundary false size >>= fun ks ->
    let len = List.length ks in
    if len = 0 
    then return acc
    else
      let acc' = acc @ ks in
      let start' = Some (last ks) in
      loop start' acc'
  in
  loop None [] >>= fun ks ->
  print_newline();
  List.iter (fun k -> Printf.printf "k:%s\n" k) ks;
  ()
    
let wrap t = OUnit.bracket setup t teardown

let suite = "Range" >::: [
  "range_all" >:: wrap range_all;
  "range_some" >:: wrap range_some;
  "range_first" >:: wrap range_first;
  "range_first_exc" >:: wrap range_first_exc;
  "range_last" >:: wrap range_last;
  "range_last_exc" >:: wrap range_last_exc;
  "range_bounded_linc" >:: wrap range_bounded_linc;
  "range_bounded_finc" >:: wrap range_bounded_finc;
  "range_bounded_linc_finc" >:: wrap range_bounded_linc_finc;
  "range_bounded_overflow_left" >:: wrap range_bounded_overflow_left;
  "range_bounded_overflow_right" >:: wrap range_bounded_overflow_right;

  "reverse_range_all" >:: wrap reverse_range_all;
  "reverse_range_some" >:: wrap reverse_range_some;
  "reverse_range_first" >:: wrap reverse_range_first;
  "reverse_range_first_exc" >:: wrap reverse_range_first_exc;
  "reverse_range_last" >:: wrap reverse_range_last;
  "reverse_range_last_exc" >:: wrap reverse_range_last_exc;
  "reverse_range_bounded_linc" >:: wrap reverse_range_bounded_linc;
  "reverse_range_bounded_finc" >:: wrap reverse_range_bounded_finc;
  "reverse_range_bounded_linc_finc" >:: wrap reverse_range_bounded_linc_finc;
  "reverse_range_bounded_overflow_left" >:: wrap reverse_range_bounded_overflow_left;
  "reverse_range_bounded_overflow_right" >:: wrap reverse_range_bounded_overflow_right;
  "fringe_lower" >:: wrap fringe_lower;
  "fringe_upper" >:: wrap fringe_upper
]
