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
open Pos
open Entry
let kps = ["xxxyyyzzz-123", out 0 1100;
           "xxxyyyzzz-235", out 0 1200;
           "xxxyyyzzz-236", out 0 1300;
           "xxxyyyzzz-237", out 0 1400;
           "xxxyyyzzz-238", out 0 1500;
           "xxxyyyzzz-239", out 0 1600;
           "xxxyyyzzz-238", out 0 1700;]

open Flog0
module MF = Flog0(Store.Sync)

let pu_leaf () = 
  let b = Buffer.create 128 in
  let h = Hashtbl.create 7 in
  let _ = MF.deflate_leaf b h kps in
  let bs = Buffer.contents b in
  let () = Printf.printf "\n%S\n" bs in
  let () = Printf.printf "bs:%i bytes\n" (String.length bs) in
  let input = MF.make_input bs 5 in (* only leaf part *)
  let leaf' = MF.inflate_leaf input in
  let leaf = kps in
  let () = OUnit.assert_equal ~printer:Leaf.leaf2s leaf leaf' in
  ()
              

let pu_index () =
  let b = Buffer.create 128 in
  let h = Hashtbl.create 7 in
  let i0 = out 0 0, kps in
  let _ = MF.deflate_index b h i0 in
  let bs = Buffer.contents b in
  let () = Printf.printf "\n%S\n" bs in
  let () = Printf.printf "bs:%i bytes\n" (String.length bs) in
  let input = MF.make_input bs 5 in (* only index part *)
  let i1 = MF.inflate_index input in
  let () = OUnit.assert_equal ~printer:Index.index2s i0 i1 in
  ()

let pu_commit() = 
  let b = Buffer.create 128 in
  let h = Hashtbl.create 7 in
  let pos = out 0 0 
  and actions = [Commit.CSet ("set0", Outer (Spindle 0, Offset 0));
                 Commit.CSet ("set1", Outer (Spindle 0, Offset 1));
                 Commit.CDelete "delete0" ]
  and now = Time.make 1L 2 false in
  let previous = Outer (Spindle 0, Offset 0) in
  let lookup = pos in
  let c0 = Commit.make_commit ~pos ~previous ~lookup now actions in
  let () = Printf.printf "com=%s\n" (Commit.commit2s c0) in
  let _ = MF.deflate_commit b h c0 in
  let bs = Buffer.contents b in
  let () = Printf.printf "\n%S\n" bs in
  let () = Printf.printf "bs:%i bytes\n" (String.length bs) in
  let input = MF.make_input bs 5 in (* only commit part *) 
  let c1 = MF.inflate_commit input in
  let () = OUnit.assert_equal ~printer:Commit.commit2s c0 c1 in
  ()

let test_metadata () =
  MF.init "test_metadata.db" Time.zero;
  let db = MF.make "test_metadata.db" in
  match MF.get_metadata db with
    | Some _ -> OUnit.assert_failure "Found metadata"
    | None -> ();
  let md = "metadata, oh metadata!" in
  MF.set_metadata db md;

  MF.close db;

  let db = MF.make "test_metadata.db" in
  match MF.get_metadata db with
    | None -> OUnit.assert_failure "Didn't find metadata"
    | Some v -> OUnit.assert_equal v md;

  MF.unset_metadata db;

  match MF.get_metadata db with
    | Some _ -> OUnit.assert_failure "Found metadata after unset"
    | None -> ();

  MF.close db;

  Unix.unlink "test_metadata.db"


let test_remake () = 
  let fn = "test_remake.db" in
  MF.init fn Time.zero;
  let log = MF.make fn in
  let slab = Slab.make Time.zero in
  let p0 = Slab.add_value slab "value0" in
  let p1 = Slab.add_leaf  slab  ["key0", p0] in
  let nil = Outer (Spindle 0, Offset 0) in
  let commit = Commit.make_commit
    ~pos:p1 ~previous:nil ~lookup:nil Time.zero [Commit.CSet ("key0", Inner 0)]
  in
  let _ = Slab.add_commit slab commit in
  MF.write log slab;
  (* deliberately don't close: metadata is out of date *)
  let log2 = MF.make fn in
  let last = MF.last log2 in
  Printf.printf "last=%s\n" (pos2s last);
  OUnit.assert_bool "last should not point to beginning of log" (last <> Outer (Spindle 0, Offset 0));
  Unix.unlink fn


let suite = 
  "Flog0" >::: [
    "pu_leaf" >:: pu_leaf;
    "pu_index" >:: pu_index;
    "pu_commit" >:: pu_commit;
    "metadata" >:: test_metadata;
    "remake" >:: test_remake;
  ]
