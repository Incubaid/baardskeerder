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

open Index
open Indexz
open OUnit
open Base

let t_neighbours () =
  let z = Loc ((out 0 7, [("j", out 0 15); ("d", out 0 14)]), []) in
  let nb = Indexz.neighbours z in
  OUnit.assert_equal (NL (out 0 14)) nb

let t_neighbours2 () =
  let z = Loc ((out 0 37,["g", out 0 21]),["m", out 0 31; "t", out 0 32]) in
  let nb = Indexz.neighbours z in
  OUnit.assert_equal (N2(out 0 37, out 0 31)) nb

let t_neighbours3 () =
  let z =  Loc ((out 0 0,["m", out 0 1; "g", out 0 2]),["t", out 0 3]) in
  let nb = Indexz.neighbours z in
  OUnit.assert_equal (N2(out 0 2,out 0 3)) nb

let t_suppress () =
  let z = Loc ((out 0 7, [("j", out 0 15); ("d", out 0 14)]), []) in
  let nb = Indexz.neighbours z in
  match nb with
    | NL (Outer (Spindle 0, Offset 14)) ->
        let z2 = Indexz.suppress L (out 0 17) None z in
        Printf.printf "z2= %s\n" (iz2s z2)
    | _ -> failwith "should be NL 14"

let t_suppress2 () =
  let z = Loc ((out 0 7,["d", out 0 8]),[]) in
  let nb = Indexz.neighbours z in
  match nb with
    | NL (Outer (Spindle 0, Offset 7)) ->
        let z2 = Indexz.suppress L (out 0 17) None z in
        Printf.printf "index = %s\n" (iz2s z2)
    | _ -> failwith "should be NL 7"

let t_suppress3 () =
  let z = Loc ((out 0 0,["m", out 0 1; "g", out 0 2]),["t", out 0 3]) in
  let r = Indexz.suppress L (out 0 4) (Some "q") z in
  let () = Printf.printf "r = %s\n" (iz2s r) in
  let e = Loc ((out 0 0,["g", out 0 4]),["q", out 0 3]) in
  OUnit.assert_equal ~printer:iz2s e r;
  ()
let t_suppress4() =
  let z =  Loc ((out 0 78, [("key_12", out 0 79)]), [("key_16", out 0 95)]) in
  let r = Indexz.suppress L (out 0 98) (Some "key_15") z in
  let e = Top (out 0 98, ["key_15", out 0 95]) in
  OUnit.assert_equal ~printer:iz2s e r

let t_split () =
  let d = 2
  and lpos = out 0 21
  and sep = "q"
  and rpos = out 0 22
  and z = Loc ((out 0 7, [("j", out 0 18); ("d", out 0 14)]), [])
  in
  let left, _ , _ = Indexz.split d lpos sep rpos z in
  OUnit.assert_equal ~printer:index2s (out 0 7, ["d",out 0 14]) left

let t_split2() =
  let d = 2
  and lpos = out 0 21
  and sep = "j"
  and rpos = out 0 22
  and z = Loc ((out 0 7, [("d", out 0 18)]), [("q", out 0 15)]) in
  let left,_,right = Indexz.split d lpos sep rpos z in
  let printer = index2s in
  OUnit.assert_equal ~printer (out 0 7,["d", out 0 21]) left;
  OUnit.assert_equal ~printer (out 0 22,["q",out 0 15]) right

let t_replace () =
  let z = Loc ((out 0 7, [("d", out 0 14)]), [("m", out 0 15)]) in
  let index = Indexz.replace (out 0 18) z in
  OUnit.assert_equal ~printer:index2s index (out 0 7,("d",out 0 18) :: ("m",out 0 15)::[])

(*
  let t_replace_with_sep () =
  let sep  = "key_12" in
  let start = 107 in
  let z = Loc ((76, [("key_10", 103)]), [("key_13", 93)]) in
  let r,_ = Index.indexz_replace_with_sep sep start z in
  let expected = 76, ["key_10", 107; "key_12", 93] in
  OUnit.assert_equal ~printer:index2s expected r
*)

let t_merge () =
  let index = out 0 110, ["key_3", out 0 93] in
  let sep = "key_3" in
  let right = out 0 94, ["key_5", out 0 64 ; "key_7", out 0 54] in
  let m = "can't merge:(Outer (0, 110), [\"key_3\", Outer (0, 93)]) "^
    "\"key_3\" (Outer (0, 94), [\"key_5\", Outer (0, 64); \"key_7\", "^
    "Outer (0, 54)])"
  in
  OUnit.assert_raises
    (Failure m)
    (fun () -> index_merge index sep right)



let suite =
  "Index" >:::[
    "neighbours" >:: t_neighbours;
    "neighbours2" >:: t_neighbours2;
    "neighbours3" >:: t_neighbours3;
    "suppress"   >:: t_suppress;
    "suppress2"  >:: t_suppress2;
    "suppress3"  >:: t_suppress3;
    "suppress4"  >:: t_suppress4;
    "split"      >:: t_split;
    "split2"     >:: t_split2;
    "replace"    >:: t_replace;
    (* "replace_with_sep" >:: t_replace_with_sep; *)
    "merge"      >:: t_merge;
  ]
