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

open Dot
open Tree
open Entry

module MDB = DB(Mlog);;

module MDot = Dot(Mlog);; 

let t0 = Mlog.make 40;;
let kvs =   [
  "a", "A";
  "d", "D";
  (* "g", "G"; *)
  (* "j", "J"; *)
   (* 
      "m", "M";
      "q", "Q";
   *)
  ];;

let () = List.iter (fun (k,v) -> MDB.set t0 k v) kvs;;
let check () = List.iter (fun (k,v) -> assert (MDB.get t0 k =v)) kvs;;
(* now delete "q" "Q" *)


