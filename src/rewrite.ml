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

open Log
open Pos
open Entry
open Base
open Dbx

module Rewrite (L0:LOG) (L1:LOG) = struct
  type update = { mutable kvs: (k * v) list; mutable w: int} 

  module DBX1 = DBX(L1)
    
  let max_w = 1024 * 1024
  let make_update () = {kvs = []; w = 0}
  let add_kvs u ((k,v) as kv) = 
    u.kvs <- kv :: u.kvs ;
    u.w <- u.w + (String.length k) + (String.length v)
      
  let update_w u = u.w
    
  let rewrite (l0:L0.t) (root0:pos) (l1:L1.t) = 
    let apply_update u = 
      DBX1.with_tx l1
	(fun tx ->
	  List.iter (fun (k,v) -> DBX1.set tx k v) u.kvs)
    in
    let read_value pos = 
      let e = L0.read l0 pos in
      match e with
	| Value v -> v
	| NIL | Leaf _ | Commit _ | Index _ -> failwith "value!"
    in
    let fat u = update_w u  > max_w in
    let rec walk acc pos =
      let e0 = L0.read l0 pos in
      match e0 with
	| NIL -> acc
	| Value _ -> acc
	| Leaf leaf -> walk_leaf acc leaf
	| Index index -> walk_index acc index
	| Commit c -> let pos = Commit.get_pos c in walk acc pos
    and walk_leaf acc leaf = 
      let rec loop acc = function
	| [] -> acc
	| (k,pos)::t -> let v = read_value pos in
			let () = add_kvs acc (k,v) in
			loop acc t
      in
      let acc1 = loop acc leaf in
      let acc2 = 
	if fat acc1 
	then 
	  let () = apply_update acc1 in
	  make_update ()
	else
	  acc1
      in
      acc2
    and walk_index acc (p0,kps) = 
      let rec loop acc p = function
	| [] -> walk acc p
	| (_,pk) :: t -> let acc' = walk acc p in
			 loop acc' pk t
      in
      loop acc p0 kps
    in
    let u0 = make_update () in
    let u = walk u0 root0 in
    apply_update u
  
  end
