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
open Dbx
open Tree
module MDBX = DBX(Mlog)
module MDB = DB(Mlog)
open Test_helper


let _setup () = 
  let fn = "bla" in
  let () = Mlog.init ~d:2 Time.zero fn in
  Mlog.make2 ~n_spindles:1 fn Time.zero

let get_after_delete () = 
  let mlog = _setup() in
  let () = MDBX.with_tx mlog (fun tx -> MDBX.set tx "a" "A") in
  let test tx = 
    MDBX.delete tx "a";
    MDBX.get tx "a"
  in
  OUnit.assert_equal ~printer:vo2s None (MDBX.with_tx mlog test)


let get_after_log_update () =
  let mlog = _setup () in
  let k = "a" 
  and v = "A" in
  let () = MDBX.log_update mlog (fun tx -> MDBX.set tx k v) in
  let test = MDB.get mlog k in
  OUnit.assert_equal ~printer:vo2s None test

let get_after_log_updates() = 
  let mlog = _setup() in
  let k = "a"
  and v = "A" in
  let () = MDBX.log_update mlog (fun tx-> MDBX.set tx k v) in
  let () = MDBX.log_update mlog ~diff:false (fun tx -> MDBX.set tx "a" "v1") in
  let () = MDBX.log_update mlog ~diff:false (fun tx -> MDBX.set tx "a" "v2") in
  let test = MDB.get mlog k in
  Mlog.dump mlog;
  OUnit.assert_equal ~printer:vo2s None test

let update_commit_get() =
  let mlog = _setup() in
  let k = "a" in
  let v = "A" in
  let (>>=) = Mlog.bind in
  MDBX.log_update mlog (fun tx -> MDBX.set tx k v) >>= fun () ->
  MDBX.commit_last mlog >>= fun () ->
  Mlog.dump mlog;
  MDB.get mlog k >>= fun vo2 ->
  OUnit.assert_equal ~printer:vo2s vo2 (Some v)
  
let suite = "DBX" >::: ["get_after_delete" >:: get_after_delete;
                        "get_after_log_update" >:: get_after_log_update;
                        "get_after_log_updates" >:: get_after_log_updates;
                        "update_commit_get" >:: update_commit_get;
                       ]
