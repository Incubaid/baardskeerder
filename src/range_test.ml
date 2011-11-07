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
  let log = Mlog.make () in
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

let wrap t = OUnit.bracket setup t teardown

let suite = "Range" >::: [
  "range_all" >:: wrap range_all;
  "range_some" >:: wrap range_some;
  "range_first" >:: wrap range_first;
  "range_first_exc" >:: wrap range_first_exc;
  "range_last" >:: wrap range_last;
  "range_last_exc" >:: wrap range_last_exc;
]
