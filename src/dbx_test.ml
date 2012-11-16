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
open Prefix
module MDBX = DBX(Mlog)
module MDB = DB(Mlog)
module MPR = Prefix(Mlog)
open Base

let (>>=) = Mlog.bind

let _setup () = 
  let fn = "bla" in
  let () = Mlog.init ~d:2 Time.zero fn in
  Mlog.make2 ~n_spindles:1 fn Time.zero

let _ok_set tx k v =
  MDBX.set tx k v >>= fun () ->
  Mlog.return (OK ())

let get_after_delete () = 
  let mlog = _setup() in
  let r0 = MDBX.with_tx mlog (fun tx -> _ok_set tx "a" "A") in
  let test tx = 
    MDBX.delete tx "a" >>= fun (OK ()) ->
    MDBX.get tx "a" >>= fun r ->
    Mlog.return r
  in
  let v2 = MDBX.with_tx mlog test in
  OUnit.assert_equal (NOK "a") v2 
    


let get_after_log_update () =
  let mlog = _setup () in
  let k = "a" 
  and v = "A" in
  let r0 = MDBX.log_update mlog (fun tx -> _ok_set tx k v) in
  let test = MDB.get mlog k in
  OUnit.assert_equal (NOK k) test

let get_after_log_updates() = 
  let mlog = _setup() in
  let k = "a"
  and v = "A" in
  MDBX.log_update mlog (fun tx-> _ok_set tx k v) >>= fun (OK()) ->
  MDBX.log_update mlog ~diff:false (fun tx -> _ok_set tx "a" "v1") >>= fun (OK())->
  MDBX.log_update mlog ~diff:false (fun tx -> _ok_set tx "a" "v2") >>= fun (OK())->
  let test = MDB.get mlog k in
  Mlog.dump mlog;
  OUnit.assert_equal (NOK k) test

let update_commit_get() =
  let mlog = _setup() in
  let k = "a" in
  let v = "A" in
  MDBX.log_update mlog (fun tx -> _ok_set tx k v) >>= fun (OK()) ->
  MDBX.commit_last mlog >>= fun () ->
  Mlog.dump mlog;
  MDB.get mlog k >>= fun vo2 ->
  OUnit.assert_equal vo2 (OK v)

let delete_empty () = 
  let mlog = _setup() in
  let k = "non-existing" in
  OUnit.assert_equal (Base.NOK k) (MDBX.with_tx mlog (fun tx -> MDBX.delete tx k))


let delete_prefix () =
  let mlog = _setup () in
  MDBX.with_tx mlog 
    (fun tx ->
      let rec loop i =
        if i = 16
        then Mlog.return (OK ())
        else
          let k = Printf.sprintf "a%03i" i in
          let v = "X" in
          _ok_set tx k v >>= fun (OK()) ->
          loop (i+1)
      in
      loop 0
    );
  let prefix = "a00" in
  MDBX.with_tx mlog (fun tx -> 
    MDBX.delete_prefix tx prefix 
    >>= fun c -> Mlog.return (OK c)) 
  >>= function 
  | OK c -> OUnit.assert_equal ~printer:string_of_int 10 c
  | NOK _ -> failwith "can't happen"
  ()

  
let log_nothing () = 
  let mlog = _setup() in
  let ok = OK () in
  let x = MDBX.log_update mlog (fun tx -> Mlog.return ok) in
  OUnit.assert_equal ok x;
  ()


let log_bug2() =
  let mlog = _setup() in
  let _ = MDBX.log_update mlog (fun tx -> _ok_set tx "k" "v") in
  let ok = OK () in
  let ok2 = MDBX.log_update mlog (fun tx -> Mlog.return ok) in
  ()

let suite = "DBX" >::: [
  "get_after_delete" >:: get_after_delete;
  "get_after_log_update" >:: get_after_log_update;
  "get_after_log_updates" >:: get_after_log_updates;
  "update_commit_get" >:: update_commit_get;
  "delete_empty" >:: delete_empty;
  "delete_prefix" >:: delete_prefix;
  "log_nothing" >:: log_nothing;
  "log_bug2" >:: log_bug2;
]
