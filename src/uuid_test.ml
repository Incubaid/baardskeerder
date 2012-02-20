(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2012 Incubaid BVBA
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
open Uuid

let test_uuid_generate () =
  let _ = uuid_generate () in
  ()

let test_uuid_unparse_upper () =
  let u = uuid_generate () in
  let s = uuid_unparse_upper u in
  assert_equal (String.length s) 36

let test_not_equal () =
  let u1 = uuid_generate ()
  and u2 = uuid_generate () in

  let s1 = uuid_unparse_upper u1
  and s2 = uuid_unparse_upper u2 in

  assert_bool (Printf.sprintf "%s != %s" s1 s2) (s1 != s2)


let suite = "UUID" >::: [
  "uuid_generate" >:: test_uuid_generate;
  "uuid_unparse_upper" >:: test_uuid_unparse_upper;
  "not_equal" >:: test_not_equal;
]
