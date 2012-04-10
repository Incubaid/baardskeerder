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

open Entry
open Tree
open Flog

open Monad

module MyFlog = Flog(Store.Sync)

open MyFlog

module M = Monad(Store.Sync)

let (>>=) = Store.Sync.bind
and return = Store.Sync.return
and run = Store.Sync.run

let test_uintN wf rf l n () =
  let m = 0x40000000 - 1 in
  let r = min n m in

  let rec loop = function
    | -1 -> ()
    | i ->
        let v = Random.int r in

        let v = if l < 8
          then v
          else
            if v > 0x3FFFFFFF then (0x3FFFFFFF00000000 + v)
            else (v lsl 32) + v
        in

        let b = Buffer.create l in
        wf (fun x -> x) b v;

        let s = Buffer.contents b in
        let l' = String.length s in
        OUnit.assert_equal ~printer:string_of_int l l';

        let s' = String.create (l' + 10) in
        let o = Random.int 10 in
        String.blit s 0 s' o l';

        OUnit.assert_equal ~printer:string_of_int v (fst (rf s' o));
        loop (pred i)
  in
  loop 1000

let base_serialization =
  "base_serialization" >::: [
    "uint8" >:: test_uintN Binary.write_uint8 Binary.read_uint8 1 0xFF;
    "uint32" >:: test_uintN Binary.write_uint32 Binary.read_uint32 4 0xFFFFFFFF;
    "uint64" >:: test_uintN Binary.write_uint64 Binary.read_uint64 8 max_int;
  ]

(* TODO Functions are hidden now... Move into parent module?
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
  and value = Value "This is a test value"
  and commit = Commit 54321 in

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
  OUnit.assert_equal ~printer:Entry.entry2s value value';
  OUnit.assert_equal ~printer:commit2s commit commit'

let entries =
  "entries" >::: [
    "serialization" >:: test_serialize_deserialize_entries;
  ]
*)

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

let test_database_create fn = run (
  init fn Time.zero >>= fun () ->
  let s = Unix.stat fn in
  return (OUnit.assert_equal (s.st_size = 2 * 4096))
)

let test_database_make fn = run (
  init fn Time.zero >>= fun () ->
  make fn >>= fun db ->
  close db
)


module FDB = DB(MyFlog)

let with_database f =
  let f' fn =
    init fn Time.zero >>= fun () ->
    make fn >>= fun db ->

    try
      f fn db >>= fun () ->
      close db
    with e ->
      close db >>= fun () ->
      raise e
  in
  with_tempfile f'

let test_database_set _ db =
  FDB.set db "foo" "bar"

let test_database_set_get _ db =
  let k = "foo"
  and v = "bar" in

  FDB.set db k v >>= fun () ->
  FDB.get db k >>= fun v' ->

  return (OUnit.assert_equal v v')

let test_database_multi_action _ db =
  let k1 = "foo"
  and v1 = "bar"
  and v1' = "bal"
  and k2 = "bat"
  and v2 = "baz" in

  FDB.set db k1 v1 >>= fun () ->
  FDB.set db k2 v2 >>= fun () ->
  let my_get k = FDB.get db k in

  my_get k1 >>= fun v1b ->
  OUnit.assert_equal v1 v1b;

  my_get k2 >>= fun v2b ->
  OUnit.assert_equal v2 v2b;

  FDB.set db k1 v1' >>= fun () ->
  my_get k1 >>= fun v1'b ->
  OUnit.assert_equal v1' v1'b;

  FDB.delete db k2 >>= fun () ->
  return (OUnit.assert_raises (Base.NOT_FOUND k2) (fun () -> my_get k2))



let test_database_reopen fn db =
  let k1 = "foo"
  and v1 = "bar"
  and k2 = "bat"
  and v2 = "baz" in

  FDB.set db k1 v1 >>= fun () ->
  FDB.set db k2 v2 >>= fun () ->

  MyFlog.close db >>= fun () ->

  make fn >>= fun db' ->
  let my_get k = FDB.get db'  k in
  my_get k1 >>= fun v1' ->
  my_get k2 >>= fun v2' ->

  OUnit.assert_equal v1 v1';
  OUnit.assert_equal v2 v2';

  close db'

let test_database_sync fn db =
  let k = "foo"
  and v = "bar" in

  FDB.set db k v >>= fun () ->

  (* Set both metadata field *)
  MyFlog.sync db >>= fun () ->
  MyFlog.sync db >>=  fun () ->
  let my_get k = FDB.get db k in

  my_get k >>= fun v' ->
  OUnit.assert_equal v' v;

  close db >>= fun () ->

  make fn >>= fun db' ->

  FDB.get db' k >>= fun v' ->

  OUnit.assert_equal v' v;

  close db'

let database =
  "database" >::: [
    "create" >:: with_tempfile test_database_create;
    "make" >:: with_tempfile test_database_make;
    "set" >:: with_database test_database_set;
    "set_get" >:: with_database test_database_set_get;
    "reopen" >:: with_database test_database_reopen;
    "multi_action" >:: with_database test_database_multi_action;
    "sync" >:: with_database test_database_sync;
  ]

let dump_fiemap f =
  let fd = openfile f [O_RDONLY] 0o644 in
  let m = Posix.ioctl_fiemap fd in

  Printf.printf "\n";
  Printf.printf "Mappings for %s:\n" f;
  Printf.printf
    "#   Logical          Physical         Length           Flags\n";
  Printf.printf
    "------------------------------------------------------------\n";
  let rec loop i = function
    | [] -> ()
    | (m :: ms) ->
        let (l, p, s, f) = m in
        Printf.printf "%02d: %-16.16Lx %-16.16Lx %-16.16Lx %-4.4lx\n"
          i l p s f;
        loop (i + 1) ms
  in
  loop 0 m

let test_compaction_basic fn db =
  let kps = [("foo", "bar"); ("baz", "bat"); ("foo", "bal")] in

  M.iter (fun (k, v) -> FDB.set db k v) kps >>= fun () ->

  MyFlog.compact ~min_blocks:1 db >>= fun () ->

  close db >>= fun () ->

  make fn >>= fun db' ->
  let id x = x in
  let my_get k = FDB.get db' k in

  my_get "foo" >>= fun v ->
  OUnit.assert_equal ~printer:id v "bal";
  close db' >>= fun () ->

  dump_fiemap fn;

  return ()

let test_compaction_lengthy fn db =
  let rec loop = function
    | 0 -> return ()
    | n ->
        let key = Printf.sprintf "key_%d" n
        and value = Printf.sprintf "value_%d" n in

        FDB.set db key value >>= fun () ->

        loop (pred n)
  in

  loop 1000 >>= fun () ->

  let do_compact () =
    MyFlog.compact ~min_blocks:1 db
  in

  do_compact () >>= fun () ->

  let rec loop2 = function
    | 0 -> return ()
    | n ->
        let key = Printf.sprintf "key_%d" n in
        FDB.delete db key >>= fun () ->
        loop2 (pred n)
  in

  loop2 1000 >>= fun () ->

  do_compact () >>= fun () ->

  dump_fiemap fn;

  return ()

let test_compaction_all_states m c fn db =
  let rec insert_loop = function
    | 0 -> return ()
    | n ->
        let key = Printf.sprintf "key_%d" n
        and value = Printf.sprintf "value_%d" n in

        FDB.set db key value >>= fun () ->

        insert_loop (pred n)
  in

  insert_loop c >>= fun () ->

  let rec test_loop t =
    let rec check_deleted n = function
      | i when i = (n - 1) -> return ()
      | i ->
          let key = Printf.sprintf "key_%d" i in
          OUnit.assert_raises (Base.NOT_FOUND key) (fun () -> FDB.get db key);
          check_deleted n (pred i)
    in

    let rec check_existing = function
      | 0 -> return ()
      | i ->
          let key = Printf.sprintf "key_%d" i
          and value = Printf.sprintf "value_%d" i in
          FDB.get db key >>= fun value' ->
          OUnit.assert_equal value value';
          check_existing (pred i)
    in

    function
      | 0 -> return ()
      | n ->
          MyFlog.compact ~min_blocks:m db >>= fun () ->
          let key = Printf.sprintf "key_%d" n in
          FDB.delete db key >>= fun () ->
          check_deleted n t >>= fun () ->
          check_existing (pred n) >>= fun () ->
          test_loop t (pred n)
  in

  test_loop c c >>= fun () ->

  dump_fiemap fn;

  return ()

let compaction =
  "compaction" >::: [
    "basic" >:: with_database test_compaction_basic;
    "lengthy" >:: with_database test_compaction_lengthy;
    "all_states_1_block" >:: with_database (test_compaction_all_states 1 1000);
    "all_states_0_blocks" >:: with_database (test_compaction_all_states 0 100);
    "all_states_64_blocks" >::
      with_database (test_compaction_all_states 64 1000);
  ]

let suite =
  "Flog" >::: [
    base_serialization;
(*    metadata;
    entries; *)
    database;
    compaction;
  ]
