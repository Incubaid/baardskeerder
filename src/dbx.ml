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

open Base
open Tree
open Log
open Entry

module DBX(L:LOG) = struct

  type tx = { log: L.t; 
	      slab: L.slab; 
	      info: (k,v option) Hashtbl.t;
	      mutable last:pos
	    }

  module DBL = DB(L)

  let get tx k = 
    if Hashtbl.mem tx.info k then
      match Hashtbl.find tx.info k with
	| None -> raise (NOT_FOUND k)
	| Some v -> v
    else
      DBL.get tx.log k

  let set tx k v = 
    Hashtbl.replace tx.info k (Some v);
    let last' = DBL._set tx.log tx.slab k v in
    let () = tx.last <- last' in
    ()
      
  let delete tx k = 
    Hashtbl.replace tx.info k None;
    let last' = DBL._delete tx.log tx.slab k in
    let () = tx.last <- last' in
    ()

  let with_tx log f = 
    let slab = L.make_slab log in
    let info = Hashtbl.create 127 in
    let last = L.last log in
    let tx = {log;slab;info;last} in
    let () = f tx in
    let c = Commit tx.last in
    let _ = L.add tx.slab c in
    L.write log tx.slab

end
