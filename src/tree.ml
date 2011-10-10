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
open Log

type trail = (dir * entry) list

exception ExTrail of trail * string
let etrail t s= raise (ExTrail (t,s))

exception ExLeaf of kps * string
let eleaf kps s = raise (ExLeaf(kps,s))

module DB = functor (L:LOG) -> struct

  let get log k = 
    let rec descend pos = 
      let e = L.read log pos in
      match e with
	| NIL -> raise Not_found
	| Value v -> v
	| Index (pp,kps) -> 
	  let (_,pos) = index_dir k pp kps in
	  descend pos
	| Leaf kps -> 
	  match leaf_dir k kps with
	    | INSERT           -> raise Not_found
	    | REPLACE (_, pos) -> descend pos
    in
    descend (L.root log)

  let set log k v = 
    let rec build_set start trail =
      let vv = Value v in
      let start' = start + L.size vv in
      vv :: do_start start start' trail 
    and do_start vpos start trail = 
      match trail with 
	| [] -> [Leaf [k,vpos]] 
	| (ir, Leaf kps) :: rest -> 
	  begin
	    match ir with
	      | INSERT -> 
		let kps' = leaf_insert k vpos kps in
		if leaf_overflow kps' 
		then
		  let l,sep,r = leaf_split kps' in
		  let lpos = vpos + (leaf_size l) in
		  let rpos = lpos + (leaf_size r) in
		  Leaf l :: Leaf r :: do_rest_overflow l lpos sep r rpos rest
		else
		  let lpos = vpos + (leaf_size kps') in
		  Leaf kps' :: do_rest lpos rest
	      | REPLACE (i,p0) -> 
		let kps' = leaf_replace i vpos kps in
		let lpos = vpos + (leaf_size kps') in
		Leaf kps' :: do_rest lpos rest
	  end
	| _ -> etrail trail "do_start"
    and do_rest_overflow l lpos sep r rpos rest = 
      match rest with
	| [] -> [Index (lpos,[sep,rpos])] 
	| (REPLACE (i0,p0), Index (pp,kps)) :: t -> 
	  let index' = index_insert i0 lpos sep rpos pp kps in
	  if index_overflow index' then
	    etrail rest "index_overflow"
	  else
	    let index_pos = rpos + leaf_size r in
	    let start = index_pos + index_size index' in
	    Index index' :: do_rest start t
    and do_rest lpos trail = 
      match trail with
	| [] -> []
	| [REPLACE (i,pos), Index (pp,kps) ] -> 
	  let index' = index_replace i lpos pp kps in
	  [Index (index')]
	| index :: rest -> etrail trail "do_rest"
    in
    
    let rec descend trail pos = 
      let e = L.read log pos in
      match e with
	| NIL -> trail
	| Value v -> failwith "corrupt"
	| Leaf kps -> 
	  let x= leaf_dir k kps in
	  (x,e) :: trail 
	| Index (pp,kps) -> 
	  let (d,p) as x = index_dir k pp kps in
	  let trail' = (REPLACE x,e) :: trail in
	  descend trail' p
    in
    let trail  = descend [] (L.root log) in
    let update = build_set (L.next log) trail in
    L.write log update

  let delete log k =
    let build_delete start trail = 
      match trail with
	| [] -> failwith "build_delete empty trail???"
	| [REPLACE (i0,p0) , Leaf kps] -> [Leaf (leaf_remove i0 kps) ]
	| [REPLACE (i0,p0) , Leaf lkps; 
	   REPLACE (i1,p1) , Index (pp,ikps)] ->
	  let lkps1 = leaf_remove i0 lkps in
	  if leaf_underflow lkps1 
	  then 
	    begin
	      let me = p1 in
	      let sibling_p,lr = pick_sibling i0 me pp ikps in
	      let sibling   = L.read log sibling_p in
	      match sibling with
		| Leaf sibling_kps -> 
		  let lkps2 = leaf_merge lr lkps1 sibling_kps in
		  if  (index_n_seps pp ikps) = 1 
		  then [Leaf lkps2]
		  else let (pp',kps') = index_remove i0 start pp ikps in
		       [Leaf lkps2; Index (pp', kps')]
		| _ -> failwith "corrupt"
	    end
	  else etrail trail "no_underflow"
	    
	| _ -> etrail trail "build_delete"
    in
    let rec descend trail pos = 
      let e = L.read log pos in
      match e with
	| NIL -> raise Not_found
	| Value v -> failwith "corrupt"
	| Leaf kps -> 
	  begin
	    match leaf_dir k kps with
	      | INSERT -> raise Not_found
	      | x      -> (x,e) :: trail
	  end
	| Index (pp, kps) -> 
	  let (i,pos) = index_dir k pp kps in
	  let x = REPLACE (i,pos) in
	  let trail' = (x,e) :: trail in
	  descend trail' pos
    in
    let trail = descend [] (L.root log) in
    let update = build_delete (L.next log) trail in
    L.write log update
end
