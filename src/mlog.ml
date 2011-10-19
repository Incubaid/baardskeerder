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

open Entry

type t = { es : entry array; mutable next:int}
    
let make cap = {es = Array.make cap NIL; next = 0}
let write t es = 
  let do_one e = 
    t.es.(t.next) <- e;
    t.next <- t.next + 1
  in
  List.iter do_one es
    
let root t = t.next -1
let next t = t.next
let size e = 1
let read t pos = if pos < 0 then NIL else t.es.(pos)

let dump (t:t) = Array.iteri 
  (fun i a -> 
    let s = 
      match a with
	| NIL     -> "NIL"
	| Value v -> Printf.sprintf "Value %S" v
	| Leaf l  -> Printf.sprintf "Leaf %s  " (Leaf.leaf2s l)
	| Index i -> Printf.sprintf "Index %s)" (Index.index2s i)
    in
    Printf.printf "%2i: %s\n" i s) 
  t.es
