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

let range_0 () = 
  let log = Mlog.make () in
  List.iter (fun k -> MDB.set log k (String.uppercase k)) ["a";"b";"c";"d";"e";"f";"g"] ;
  let r0 = MDB.range log None true None true None in
  let printer r = Pretty.string_of_list (fun s -> s) r in
  OUnit.assert_equal ~printer ["a";"b";"c";"d";"e";"f";"g"] r0;;

let suite = "Range" >::: ["range_0" >:: range_0]
