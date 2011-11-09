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
module F2db = DB(Flog2)

let () =
  let fn = "/tmp/flog2.db" in
  let log = Flog2.make fn in
  List.iter (fun k -> F2db.set log k (String.uppercase k)) ["a" ; "d"; "g";"j";"m";"q";"t";"w";"z"];
  Flog2.close log;
  let log2 = Flog2.make fn in
  let () = Flog2.dump log2 in
  Flog2.close log2
  
