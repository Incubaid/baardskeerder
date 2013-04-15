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

open Indexz
open OUnit
open Pos

let assert_balanced z =
  let m = Printf.sprintf "not balanced: %s" (iz2s z) in
  match z with
    | (_, (_::_ as l), r) ->
        let ls = List.length l in
        let rs = List.length r in
        let b = (ls = rs + 1) in
        OUnit.assert_bool m b
    | (_, [], _) -> OUnit.assert_bool m false

let t_balance() =
  let d = 2 in
  let z = (out 0 7,["q", out 0 22; "j", out 0 21; "d", out 0 14],[]) in
  let z' = Indexz.balance d z in
  assert_balanced z'


let t_balance2 () =
  let d = 2 in
  let z = make_indexz (out 0 0,["d", out 0 1; "j", out 0 2; "q", out 0 3]) in
  let z' = Indexz.balance d z in
  assert_balanced z'

let t_balance3 () =
  let d = 3 in
  let z = make_indexz (out 0 0,["d", out 0 1;
                        "j", out 0 2;
                        "q", out 0 3;
                        "t", out 0 4;
                        "w", out 0 5])
  in
  let z' = Indexz.balance d z in
  assert_balanced z'

let t_balance4() =
  let d = 3 in
  let z = (out 0 16,
           ["key_93", out 0 43;
            "key_90", out 0 56;
            "key_87", out 0 69;
            "key_102", out 0 68],
           ["key_96",out 0 30]) in
  let z' = Indexz.balance d z in
  assert_balanced z'

let t_balance5() =
  let d = 3 in
  let z =
    (out 0 74,
     [("key_63", out 0 271);
      ("key_109", out 0 270)],
     [("key_72", out 0 222);
      ("key_81", out 0 173);
      ("key_90", out 0 124)]) in
  let z' = Indexz.balance d z in
  assert_balanced z'

let suite = "Indexz" >::: [
  "balance"    >:: t_balance;
  "balance2"   >:: t_balance2;
  "balance3"   >:: t_balance3;
  "balance4"   >:: t_balance4;
  "balance5"   >:: t_balance5;
]
