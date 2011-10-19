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
    and descend_index trail index = 
      let z = index_find_set index k in
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
	  let left, (sep,ps) , right = leafz_split k start z in
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
    and set_overflow lpos sep rpos trail = 
      match trail with 
      | [] -> [Index (lpos, [sep,rpos])]
      | Index_down z :: rest -> 
	if indexz_max z 
	then 
	  let left, sep', right  = indexz_split lpos sep rpos z in
	  let lpos' = rpos + 1 in
	  let rpos' = lpos' + 1 in
	  Index left :: Index right :: set_overflow lpos' sep' rpos' rest
	else
	  let z' = indexz_insert lpos sep rpos z in
	  let i' = indexz_close z' in
	  let start' = rpos + 1 in
	  Index i' :: set_rest start' rest
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
	| Leaf l -> descend_leaf trail l
	| Index i -> descend_index trail i
    and descend_leaf trail leaf = 
      match Base.leaf_find_delete leaf k with
	| None -> raise Not_found
	| Some (p,z) -> 
	  let step = Leaf_down z in 
	  descend p (step::trail)
    and descend_index trail index = 
      let z = index_find_set index k in
      let trail' = Index_down z :: trail in
      let pos' = indexz_pos z in
      descend pos' trail'
    and delete_start start trail = match trail with
      | [] -> failwith "corrupt" 
      | [Leaf_down z ]-> [Leaf (leafz_delete z)]
      | Leaf_down z :: rest ->
	if leafz_min z 
	then leaf_underflow start z rest
	else 
	  Leaf (leafz_delete z) :: delete_rest (start + 1) rest
    and delete_rest start trail = match trail with
      | [] -> []
      | _ -> failwith "todo"
    and leaf_underflow start leafz rest = 
      match rest with 
	| [] -> [Leaf (leafz_delete leafz)]
	| Index_down z :: rest -> 
	  begin
	    match indexz_neighbours z with
	      | NR pos     -> 
		begin
		  let r = L.read t pos in
		  match r with
		    | Leaf right -> 
		      if leaf_min right
		      then 
			begin
			  let left = leafz_delete leafz in
			  Printf.printf "left=%s  right=%s\n" (leaf2s left) (leaf2s right);
			  let h  =  Leaf (leaf_merge left right) in
			  let hpos = start + 1 in
			  let z' = indexz_suppress R hpos z in
			  let t = leaves_merged z' rest in
			  h :: t
			end
		      else failwith "??"
		end
	      | N2 (p0,p1) -> failwith "n2"
	  end
	| _ -> failwith "corrupt"
    and leaves_merged z rest = 
      match z, rest with
	| (_,[]) , [] -> []
	| _ -> failwith "leaves_merged"
    in
    let trail = descend (L.root t) [] in
    let update = delete_start (L.next t) trail in
    L.write t update
end 
