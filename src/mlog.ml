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

