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
open Leaf
open Index

module DB = functor (L:LOG ) -> struct
  let d = 2

  let get (t:L.t) k = 
    let rec descend pos = 
      let e = L.read t pos in
      match e with
	| NIL -> raise (NOT_FOUND k)
	| Value v -> v
	| Leaf l -> descend_leaf l
	| Index i -> descend_index i
    and descend_leaf = function
      | [] -> raise (NOT_FOUND k)
      | (k0,p0) :: t -> 
	if k= k0 then descend p0 else
	  if k > k0 then descend_leaf t
	  else raise (NOT_FOUND k)
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



  let _set (t:L.t) slab k v = 
    let add_value s v = L.add s (Value v) in
    let add_leaf  s l = L.add s (Leaf l) in
    let add_index s i = L.add s (Index i) in
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
    let rec set_start slab start trail = 
      match trail with 
      | [] -> let vpos = add_value slab v in
	      let _    = add_leaf  slab [k,vpos] in
	      ()
      | Leaf_down z :: rest -> 
	if leafz_max d z 
	then 
	  let left, (sep,ps) , right = leafz_split d k start z in
	  let _    = add_value slab v     in
	  let lpos = add_leaf  slab left  in
	  let rpos = add_leaf  slab right in
	  set_overflow slab lpos sep rpos rest
	else
	  let l = leafz_insert k start z in
	  let _    = add_value slab v    in
	  let lpos = add_leaf  slab l    in
	  set_rest slab lpos rest 
    and set_rest (slab:L.slab) start = function
      | [] -> ()
      | (Index_down z) :: rest -> 
	let index = indexz_replace start z in
	let ipos = add_index slab index    in
	set_rest slab ipos rest
    and set_overflow (slab:L.slab) lpos sep rpos trail = 
      match trail with 
      | [] -> let _ = add_index slab  (lpos, [sep,rpos]) in ()
      | Index_down z :: rest -> 
	if indexz_max d z 
	then 
	  let left, sep', right  = indexz_split d lpos sep rpos z in
	  let lpos' = add_index slab left  in
	  let rpos' = add_index slab right in
	  set_overflow slab lpos' sep' rpos' rest
	else
	  let z' = indexz_insert lpos sep rpos z in
	  let i' = indexz_close z' in
	  let start' = add_index slab i' in
	  set_rest slab start' rest
    in
    let trail = descend_set (L.root t) [] in
    set_start slab (L.next t) trail


  let set (t:L.t) k v =
    let slab = L.make_slab t in
    let () = _set t slab k v in
    L.write t slab

  let _delete (t:L.t) slab k = 
    (* let () = Printf.eprintf "delete %S\n%!" k in *)
    let add_leaf  s l = L.add s (Leaf l) in
    let add_index s i = L.add s (Index i) in
    let rec descend pos trail = 
      let e = L.read t pos in
      match e with
	| NIL -> failwith "corrupt"
	| Value v -> trail
	| Leaf l -> descend_leaf trail l
	| Index i -> descend_index trail i
    and descend_leaf trail leaf = 
      match leaf_find_delete leaf k with
	| None -> raise (NOT_FOUND k)
	| Some (p,z) -> 
	  let step = Leaf_down z in 
	  descend p (step::trail)
    and descend_index trail index = 
      let z = index_find_set index k in
      let trail' = Index_down z :: trail in
      let pos' = indexz_pos z in
      descend pos' trail'
    and delete_start slab start trail = 
      match trail with
      | [] -> failwith "corrupt" 
      | [Leaf_down z ]-> let _ = add_leaf slab (leafz_delete z) in ()
      | Leaf_down z :: rest ->
	if leafz_min d z 
	then leaf_underflow slab start z rest
	else 
	  let leaf' = leafz_delete z in
	  let lpos = add_leaf slab leaf' in
	  assert (lpos = start);
	  delete_rest slab start rest
    and delete_rest slab start trail = match trail with
      | [] -> ()
      | Index_down z :: rest -> 	
	let index = indexz_replace start z in
	let ipos = add_index slab index in
	delete_rest slab ipos rest

    and leaf_underflow slab start leafz rest = 
      match rest with 
	| [] -> let _ = add_leaf slab (leafz_delete leafz) in ()
	| Index_down z :: rest -> 
	  begin
	    let read_leaf pos = 
	      let e = L.read t pos in
	      match e with
		| Leaf l -> l
		| _ -> failwith "should be leaf"
	    in
	    let nb = indexz_neighbours z in
	    match nb with
	      | NR pos     -> 
		begin
		  let right = read_leaf pos in
		  if leaf_min d right
		  then 
		    begin
		      let left = leafz_delete leafz in
		      let h  =  leaf_merge left right in
		      let sep_c = leaf_max_key h in
		      let hpos = add_leaf slab h in
		      let z' = indexz_suppress R hpos sep_c z in
		      xxx_merged slab hpos sep_c z' rest
		    end
		  else (* borrow from right *)
		    begin
		      let left = leafz_delete leafz in
		      let left', sep', right' = leaf_borrow_right left right in
		      let lpos = add_leaf slab left' in
		      let rpos = add_leaf slab right' in
		      xxx_borrowed_right slab lpos sep' rpos z rest
		    end
		end
	      | NL pos ->
		begin
		  let left = read_leaf pos in
		  if leaf_min d left 
		  then
		    begin
		      let right = leafz_delete leafz in
		      let h = leaf_merge left right  in
		      let sep_c = leaf_max_key h in (* this might become a separator *)
		      let hpos = add_leaf slab h in
		      let index' = indexz_suppress L hpos sep_c z in
		      xxx_merged slab hpos sep_c index' rest
		    end
		  else (* borrow from left *)
		    begin
		      let right = leafz_delete leafz in
		      let left', sep',right' = leaf_borrow_left left right in
		      let lpos = add_leaf slab left' in
		      let rpos = add_leaf slab right' in
		      xxx_borrowed_left slab lpos sep' rpos z rest
		    end
		end
	      | N2 (p0,p1) -> 
		begin
		  let left = read_leaf p0 in
		  let right = read_leaf p1 in
		  match (leaf_mergeable d left, leaf_mergeable d right) with
		    | true,_ ->
		      begin
			let right = leafz_delete leafz in
			let h = leaf_merge left right in
			let sep_c = leaf_max_key h in
			let hpos = add_leaf slab h in
			let index' = indexz_suppress L hpos sep_c z in
			xxx_merged slab hpos sep_c index' rest
		      end
		    | _, true ->
		      begin
			let left = leafz_delete leafz in
			let h = leaf_merge left right in
			let sep_c = leaf_max_key h in
			let hpos = add_leaf slab h in
			let index' = indexz_suppress R hpos sep_c z in
			xxx_merged slab hpos sep_c index' rest
		      end
		    | _,_ -> (* borrow from left *)
		      begin
			let right = leafz_delete leafz in
			let left', sep', right' = leaf_borrow_left left right in
			let lpos = add_leaf slab left' in
			let rpos = add_leaf slab right' in
			xxx_borrowed_left slab lpos sep' rpos z rest
		      end		    
		end
	  end
	| _ -> failwith "corrupt"
    and xxx_borrowed_right slab lpos sep rpos z rest = 
      let z' = indexz_borrowed_right lpos sep rpos z in
      let ipos = add_index slab (indexz_close z')    in
      delete_rest slab ipos rest
    and xxx_borrowed_left slab lpos sep rpos z rest = 
      let z' = indexz_borrowed_left lpos sep rpos z in
      let ipos = add_index slab (indexz_close z') in
      delete_rest slab ipos rest
    and xxx_merged slab start sep_c index rest = 
      let read_index pos = 
	let e = L.read t pos in
	match e with
	  | Index i -> i
	  | _ -> failwith "should be index"
      in
      let merge_left left index z rest = 
	begin
	  let sep = indexz_separator L z in
	  let index' = index_merge left sep index in
	  let ipos' = add_index slab index' in
	  let z2 = indexz_suppress L ipos' sep_c z in
	  xxx_merged slab ipos' sep_c z2 rest
	end
      in
      let merge_right right index z rest = 
	begin
	  let sep = indexz_separator R z in
	  let sep_f = if sep = k then sep_c else sep in
	  let index' = index_merge index sep_f right in
	  let ipos' = add_index slab index' in
	  let z2 = indexz_suppress R ipos' sep_c z in
	  xxx_merged slab ipos' sep_c z2 rest
	end
      in
      match index, rest with
	| (_,[]) , []  -> ()
	| index , [] -> let _ = add_index slab index in ()
	| index , Index_down z :: rest when index_below_min d index -> 
	  begin
	    let nb = indexz_neighbours z in
	    match nb with
	      | NL pos ->  
		begin
		  let left = read_index pos in
		  if index_mergeable d left 
		  then merge_left left index z rest
		  else (* borrow from left *)
		    let right = indexz_delete index in
		    let left', sep', right' = index_borrow_left left right in
		    let lpos = add_index slab left' in
		    let rpos = add_index slab right' in
		    xxx_borrowed_left slab lpos sep' rpos z rest
		end
		  
	      | NR pos ->  
		begin
		  let right = read_index pos in
		  if index_mergeable d right 
		  then merge_right right index z rest
		  else
		    let left',sep', right' = index_borrow_right index right in
		    let lpos = add_index slab left' in
		    let rpos = add_index slab right' in
		    xxx_borrowed_right slab lpos sep' rpos z rest
		end
		
	      | N2 (pl,pr) -> 
		let left = read_index pl in
		let right = read_index pr in
		match (index_mergeable d left,index_mergeable d right) with
		  | true,_ -> merge_left left index z rest
		  | _, true -> merge_right right index z rest
		  | _,_ -> 
		    begin
		      let s = Printf.sprintf "todo N2(%i,%i)" pl pr in failwith s
		    end 
	  end
	| _ -> let ipos = L.add slab (Index index) in
	       delete_rest slab ipos rest
    in
    let trail = descend (L.root t) [] in
    let start = L.next t in
    let () = delete_start slab start trail 
    in ()


  let delete (t:L.t) k =
    let slab = L.make_slab t in
    let () = _delete t slab k in
    L.write t slab

  let range (t:L.t) (first:k option) (finc:bool) (last:k option) (linc:bool) (max:int) = failwith "todo"
    
end 

