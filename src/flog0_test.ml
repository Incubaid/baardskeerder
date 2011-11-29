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
let kps = ["xxxyyyzzz-123", out 1100;
           "xxxyyyzzz-235", out 1200;
           "xxxyyyzzz-236", out 1300;
           "xxxyyyzzz-237", out 1400;
           "xxxyyyzzz-238", out 1500;
           "xxxyyyzzz-239", out 1600;
           "xxxyyyzzz-238", out 1700;]
let pu_leaf () = 
  let b = Buffer.create 128 in
  let h = Hashtbl.create 7 in
  let _ = Flog0.deflate_leaf b h kps in
  let bs = Buffer.contents b in
  let () = Printf.printf "\n%S\n" bs in
  let () = Printf.printf "bs:%i bytes\n" (String.length bs) in
  let input = Flog0.make_input bs 5 in (* only leaf part *)
  let leaf' = Flog0.inflate_leaf input in
  let leaf = Leaf kps in
  let () = OUnit.assert_equal ~printer:entry2s leaf leaf' in
  ()
              

let pu_index () =
  let b = Buffer.create 128 in
  let h = Hashtbl.create 7 in
  let index = out 0, kps in
  let i0 = Index index in
  let _ = Flog0.deflate_index b h index in
  let bs = Buffer.contents b in
  let () = Printf.printf "\n%S\n" bs in
  let () = Printf.printf "\bs:%i bytes\n" (String.length bs) in
  let input = Flog0.make_input bs 5 in (* only index part *)
  let i1 = Flog0.inflate_index input in
  let () = OUnit.assert_equal ~printer:entry2s i0 i1 in
  ()
  
let suite = 
  "Flog0" >::: [
    "pu_leaf" >:: pu_leaf;
    "pu_index" >:: pu_index;
  ]
