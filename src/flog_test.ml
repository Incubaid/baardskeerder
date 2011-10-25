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
open Unix

open Tree
open Flog

let test_uintN wf rf l n () =
  let rec loop = function
    | -1 -> ()
    | n ->
        let l' = l + 10 in
        let s = String.create l'
        and o = Random.int (l' - l) in
        wf n s o;
        OUnit.assert_equal ~printer:string_of_int n (rf s o);
        loop (pred n)
  in
  loop 0xFF

let base_serialization =
  "base_serialization" >::: [
    "uint8" >:: test_uintN write_uint8 read_uint8 1 0xFF;
    "uint32" >:: test_uintN write_uint32 read_uint32 4 0xFFFFFFFF;
    "uint64" >:: test_uintN write_uint64 read_uint64 8 max_int;
  ]

let test_metadata_serialization () =
  let from_some = function
    | Some x -> x
    | _ -> invalid_arg "test_metadata_serialization: from_some"
  in

  let b = 4096 in
  let md1 = { md_blocksize=b; md_spindle=0; md_offset=0; md_count=0 } in
  let md2 = { md1 with md_spindle=1; md_offset=654321; md_count=456789 } in

  let s1, b1 = serialize_metadata md1
  and s2, b2 = serialize_metadata md2 in

  OUnit.assert_equal ~printer:string_of_int b b1;
  OUnit.assert_equal ~printer:string_of_int b b2;

  let md1' = from_some (deserialize_metadata s1)
  and md2' = from_some (deserialize_metadata s2) in

  OUnit.assert_equal md1 md1';
  OUnit.assert_equal md2 md2'

let metadata =
  "metadata" >::: [
    "serialization" >:: test_metadata_serialization;
  ]

let test_serialize_deserialize_entries () =
  (* TODO This isn't a nice unit-test, since it tests multiple types/functions.
   * Some QuickCheck-like mechanism generating nodes at random, then performing
   * a comparison after serialization/deserialization might be useful.
   *)
  let kps = [("a", 1); ("b", 2); ("c", 0xFFFFFFFF); ("def", max_int)] in

  let leaf = kps
  and index = (54321, kps)
  and value = "This is a test value"
  and commit = 54321 in

  let sl = Flog.serialize_leaf leaf
  and si = Flog.serialize_index index
  and sv = Flog.serialize_value value
  and sc = Flog.serialize_commit commit in

  let leaf' = Flog.deserialize_leaf sl 5
  and index' = Flog.deserialize_index si 5
  and value' = Flog.deserialize_value sv 5
  and commit' = Flog.deserialize_commit sc 5 in

  OUnit.assert_equal ~printer:Entry.entry2s (Entry.Leaf leaf) leaf';
  OUnit.assert_equal ~printer:Entry.entry2s (Entry.Index index) index';
  OUnit.assert_equal ~printer:Entry.entry2s (Entry.Value value) value';
  OUnit.assert_equal ~printer:commit2s (Commit commit) commit'

let entries =
  "entries" >::: [
    "serialization" >:: test_serialize_deserialize_entries;
  ]

let with_tempfile f = fun () ->
  let pid = Unix.getpid () in
  let fn = Printf.sprintf "test_%d.db" pid in

  let do_unlink () =
    try
      Unix.unlink fn
    with
      Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  in

  try
    f fn;
    do_unlink ()
  with e ->
    do_unlink ();
    raise e

let test_database_create fn =
  create fn 4096;
  let s = Unix.stat fn in
  OUnit.assert_equal (s.st_size = 2 * 4096)

let test_database_make fn =
  let () = create fn 4096 in
  let db = make fn in
  close db

module MDB = DB(Flog)

let with_database f =
  let f' fn =
    create fn 4096;
    let db = make fn in

    try
      f fn db;
      close db
    with e ->
      close db;
      raise e
  in
  with_tempfile f'

let test_database_set _ db =
  MDB.set db "foo" "bar"

let test_database_set_get _ db =
  let k = "foo"
  and v = "bar" in

  MDB.set db k v;

  let v' = MDB.get db k in

  OUnit.assert_equal v v'

let test_database_multi_action _ db =
  let k1 = "foo"
  and v1 = "bar"
  and v1' = "bal"
  and k2 = "bat"
  and v2 = "baz" in

  MDB.set db k1 v1;
  MDB.set db k2 v2;

  OUnit.assert_equal v1 (MDB.get db k1);
  OUnit.assert_equal v2 (MDB.get db k2);

  MDB.set db k1 v1';
  OUnit.assert_equal v1' (MDB.get db k1);

  MDB.delete db k2;
  OUnit.assert_raises (Base.NOT_FOUND k2) (fun () -> MDB.get db k2)


let test_database_reopen fn db =
  let k1 = "foo"
  and v1 = "bar"
  and k2 = "bat"
  and v2 = "baz" in

  MDB.set db k1 v1;
  MDB.set db k2 v2;

  close db;

  let db' = make fn in
  let v1' = MDB.get db' k1
  and v2' = MDB.get db' k2 in

  OUnit.assert_equal v1 v1';
  OUnit.assert_equal v2 v2';

  close db'

let database =
  "database" >::: [
    "create" >:: with_tempfile test_database_create;
    "make" >:: with_tempfile test_database_make;
    "set" >:: with_database test_database_set;
    "set_get" >:: with_database test_database_set_get;
    "reopen" >:: with_database test_database_reopen;
    "multi_action" >:: with_database test_database_multi_action;
  ]

let suite =
  "Flog" >::: [
    base_serialization;
    metadata;
    entries;
    database;
  ]
