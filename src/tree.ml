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

(* .. *)
open Log
open Entry
open Base

module DB = functor (L:LOG ) -> struct

  let get (t:L.t) k = 
    let rec descend pos = 
      let e = L.read t pos in
      match e with
	| NIL -> raise Not_found
	| Value v -> v
	| Leaf l -> descend_leaf l
	| Index i -> descend_index i
    and descend_leaf = function
      | [] -> raise Not_found
      | (k0,p0) :: t -> 
	if k= k0 then descend p0 else
	  if k > k0 then descend_leaf t
	  else raise Not_found
    and descend_index (p0, kps) = 
      let rec loop pi = function
	| []                       -> pi
	| (ki,_) :: _ when k <= ki -> pi 
	| (_ ,p) :: t              -> loop p t
      in
      let pos' = loop p0 kps in
      descend pos'
	
    in
    descend (L.root t)


  let set (t:L.t) k v = 
    let rec descend_set pos trail = 
      let e = L.read t pos in
      match e with
	| NIL     -> []
	| Value _ -> failwith "value ?"
	| Leaf l  -> descend_leaf trail l
	| Index i -> descend_index trail i
	  
    and descend_leaf trail leaf =
      let z = leaf_find_set leaf k in
      Leaf_down z :: trail
    and descend_index trail leaf = 
      let z = index_find_set leaf k in
      let trail' = Index_down z :: trail in
      let pos' = indexz_pos z in
      descend_set pos' trail'
    in 
    let rec set_start start trail = 
      match trail with 
      | [] -> Value v :: Leaf [k,start] :: []
      | Leaf_down z :: rest -> 
	if leafz_max z 
	then 
	  let () = Printf.printf "z=%s\n" (lz2s z) in
	  let left, (sep,ps) , right = leafz_split k start z in
	  let () = Printf.printf "left=%s, (%s,%i), right=%s\n" (leaf2s left) sep ps (leaf2s right) in
	  let lpos = start + 1 in
	  let rpos = start + 2 in
	  Value v:: Leaf left :: Leaf right :: set_overflow lpos sep rpos rest
	else
	  let l = leafz_insert k start z in
	  let start' = start + 1 in
	  Value v :: Leaf l :: set_rest start' rest
    and set_rest start = function
      | [] -> []
      | (Index_down z) :: rest -> 
	let index = indexz_replace start z in
	let start' = start + 1 in
	(Index index) :: set_rest start' rest
    and set_overflow lpos sep rpos = function
      | [] -> [Index (lpos, [sep,rpos])]
      | _ -> failwith "set_overflow"
    in
    let trail = descend_set (L.root t) [] in
    let update = set_start (L.next t) trail in
    L.write t update


  let delete (t:L.t) k = 
    let rec descend pos trail = 
      let e = L.read t pos in
      match e with
	| NIL -> failwith "corrupt"
	| Value v -> trail
	| Leaf leaf -> descend_leaf trail leaf
    and descend_leaf trail leaf = 
      match Base.leaf_find_delete leaf k with
	| None -> raise Not_found
	| Some (p,z) -> 
	  let step = Leaf_down z in 
	  descend p (step::trail)
    and start_delete start trail = match trail with
      | [] -> failwith "corrupt" 
      | [Leaf_down z ]-> [Leaf (leafz_delete z)]
      | _ -> failwith "todo"
    in
    let trail = descend (L.root t) [] in
    let update = start_delete (L.next t) trail in
    L.write t update
end 
