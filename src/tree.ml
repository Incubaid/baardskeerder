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

(* 
   .. 
*)
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
      let z = Indexz.find_set index k in
      let trail' = Index_down z :: trail in
      let pos' = Indexz.pos z in
      descend_set pos' trail'
    in 
    let rec set_start slab start trail = 
      match trail with 
      | [] -> let vpos = add_value slab v in
	      let _    = add_leaf  slab [k,vpos] in
	      ()
      | Leaf_down z :: rest -> 
	if Leafz.max d z 
	then 
	  let left, (sep,_) , right = Leafz.split d k start z in
	  let _    = add_value slab v     in
	  let lpos = add_leaf  slab left  in
	  let rpos = add_leaf  slab right in
	  set_overflow slab lpos sep rpos rest
	else
	  let l = Leafz.insert k start z in
	  let _    = add_value slab v    in
	  let lpos = add_leaf  slab l    in
	  set_rest slab lpos rest 
    and set_rest (slab:L.slab) start = function
      | [] -> ()
      | (Index_down z) :: rest -> 
	let index = Indexz.replace start z in
	let ipos = add_index slab index    in
	set_rest slab ipos rest
    and set_overflow (slab:L.slab) lpos sep rpos trail = 
      match trail with 
      | [] -> let _ = add_index slab  (lpos, [sep,rpos]) in ()
      | Index_down z :: rest -> 
	if Indexz.max d z 
	then 
	  let left, sep', right  = Indexz.split d lpos sep rpos z in
	  let lpos' = add_index slab left  in
	  let rpos' = add_index slab right in
	  set_overflow slab lpos' sep' rpos' rest
	else
	  let z' = Indexz.insert lpos sep rpos z in
	  let i' = Indexz.close z' in
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
    let add_leaf  s l = L.add s (Leaf l) in
    let add_index s i = L.add s (Index i) in
    let rec descend pos trail = 
      let e = L.read t pos in
      match e with
	| NIL -> failwith "corrupt"
	| Value _ -> trail
	| Leaf l -> descend_leaf trail l
	| Index i -> descend_index trail i
    and descend_leaf trail leaf = 
      match Leafz.find_delete leaf k with
	| None -> raise (NOT_FOUND k)
	| Some (p,z) -> 
	  let step = Leaf_down z in 
	  descend p (step::trail)
    and descend_index trail index = 
      let z = Indexz.find_set index k in
      let trail' = Index_down z :: trail in
      let pos' = Indexz.pos z in
      descend pos' trail'
    and delete_start slab start trail = 
      match trail with
      | [] -> failwith "corrupt" 
      | [Leaf_down z ]-> 
	let leaf', _ = Leafz.delete z in
	let _ = add_leaf slab leaf' in ()
      | Leaf_down z :: rest ->
	if Leafz.min d z 
	then leaf_underflow slab z rest
	else 
	  let leaf',lr = Leafz.delete z in
	  let lpos = add_leaf slab leaf' in
	  assert (lpos = start);
	  delete_rest slab start lr rest
    and delete_rest slab start (lr : k option) trail = match trail with
      | [] -> ()
      | Index_down z :: rest -> 
	begin	  
	  match lr with
	    | None ->
	      let index = Indexz.replace start z in
	      let ipos = add_index slab index in
	      delete_rest slab ipos lr rest
	    | Some sep ->
	      if Indexz.can_go_right z 
	      then 
		let z' = Indexz.replace_right sep z in
		let index = Indexz.replace start z' in
		let ipos = add_index slab index in
		let lr' = None in (* since there is a larger subtree, I don't know the lr anymore *)
		delete_rest slab ipos lr' rest
	      else
		let index = Indexz.replace start z in
		let ipos = add_index slab index in
		delete_rest slab ipos lr rest
	end

    and leaf_underflow slab leafz rest = 
      match rest with 
	| [] -> 
	  let leaf',_ = Leafz.delete leafz in
	  let _ = add_leaf slab leaf' in ()
	| Index_down z :: rest -> 
	  begin
	    let read_leaf pos = 
	      let e = L.read t pos in
	      match e with
		| Leaf l -> l
		| _ -> failwith "should be leaf"
	    in
	    let nb = Indexz.neighbours z in
	    match nb with
	      | Indexz.NR pos     -> 
		begin
		  let right = read_leaf pos in
		  if leaf_min d right
		  then 
		    begin
		      let left, _  = Leafz.delete leafz in
		      let h,sep_c  =  leaf_merge left right in
		      let hpos = add_leaf slab h in
		      let z' = Indexz.suppress Indexz.R hpos sep_c z in
		      let sep_c' = if Indexz.can_go_right z' then None else sep_c in
		      let index' = Indexz.close z' in
		      xxx_merged slab sep_c' index' rest
		    end
		  else (* borrow from right *)
		    begin
		      let left, _ = Leafz.delete leafz in
		      let left', _, right' = leaf_borrow_right left right in
		      let sep_c = leaf_min_key right in
		      let lpos = add_leaf slab left' in
		      let rpos = add_leaf slab right' in
		      let () = xxx_borrowed_right slab lpos rpos z sep_c rest in
		      ()
		    end
		end
	      | Indexz.NL pos ->
		begin
		  let left = read_leaf pos in
		  if leaf_min d left 
		  then
		    begin
		      let right, _ = Leafz.delete leafz in
		      let h, sep_c = leaf_merge left right  in
		      let hpos = add_leaf slab h in
		      let z' = Indexz.suppress Indexz.L hpos sep_c z in
		      let index' = Indexz.close z' in
		      xxx_merged slab sep_c index' rest
		    end
		  else (* borrow from left *)
		    begin
		      let right, sep_c = Leafz.delete leafz in
		      let left', sep', right' = leaf_borrow_left left right in
		      let lpos = add_leaf slab left' in
		      let rpos = add_leaf slab right' in
		      xxx_borrowed_left slab lpos sep' rpos z sep_c rest
		    end
		end
	      | Indexz.N2 (p0,p1) -> 
		begin
		  let left = read_leaf p0 in
		  let right = read_leaf p1 in
		  match (leaf_mergeable d left, leaf_mergeable d right) with
		    | true,_ ->
		      begin
			let right, _ = Leafz.delete leafz in
			let h, lr = leaf_merge left right in
			let hpos = add_leaf slab h in
			let z' = Indexz.suppress Indexz.L hpos lr z in 
			let index' = Indexz.close z' in
			let lr'  = 
			  if Indexz.can_go_right z 
			  then None
			  else lr
			in
			let () = xxx_merged slab lr' index' rest in 
			()
		      end
		    | _, true ->
		      begin
			let left,_ = Leafz.delete leafz in
			let h,sep_c = leaf_merge left right in
			let hpos = add_leaf slab h in
			let z' = Indexz.suppress Indexz.R hpos sep_c z in
			let sep_c' = 
			  if Indexz.can_go_right z' then
			    None
			  else
			    sep_c
			in
			let index' = Indexz.close z' in
			let () = xxx_merged slab sep_c' index' rest in
			()
		      end
		    | _,_ -> (* borrow from left *)
		      begin
			let right, sep_c = Leafz.delete leafz in
			let left', sep', right' = leaf_borrow_left left right in
			let lpos = add_leaf slab left' in
			let rpos = add_leaf slab right' in
			xxx_borrowed_left slab lpos sep' rpos z sep_c rest
		      end		    
		end
	  end
	| _ -> failwith "corrupt"
    and xxx_borrowed_right slab lpos rpos  z (lr:k) rest = 
      let z' = Indexz.borrowed_right lpos lr rpos z in
      let lr' = if Indexz.can_go_right z' then None else Some lr in
      let index' = Indexz.close z' in
      let ipos = add_index slab index' in
      delete_rest slab ipos lr' rest
    and xxx_borrowed_left slab lpos sep rpos z lr rest = 
      match lr with
	| None -> 
	  let z2 = Indexz.borrowed_left lpos sep rpos z in
	  let index' = Indexz.close z2 in
	  let ipos' = add_index slab index' in
	  delete_rest slab ipos' None rest
	| Some s ->
	  let z2 = 
	    if Indexz.can_go_right z 
	    then Indexz.replace_right s z 
	    else z
	  in
	  let z3 = Indexz.borrowed_left lpos sep rpos z2 in
	  let lr' = if Indexz.can_go_right z3 then None else lr in
	  let index' = Indexz.close z3 in	  
	  let ipos' = add_index slab index' in
	  delete_rest slab ipos' lr' rest
    and xxx_merged slab sep_c (index:Index.index) rest = 
      let read_index pos = 
	let e = L.read t pos in
	match e with
	  | Index i -> i
	  | _ -> failwith "should be index"
      in
      let merge_left left index z rest = 
	begin
	  let sep = Indexz.separator Indexz.L z in
	  let index1 = index_merge left sep index in
	  let ipos1 = add_index slab index1 in
	  let z2 = Indexz.suppress Indexz.L ipos1 sep_c z in
	  let lr' = if Indexz.can_go_right z2 then None else sep_c in
	  let index2 = Indexz.close z2 in
	  xxx_merged slab lr' index2 rest
	end
      in
      let merge_right right index z lr rest = 
	begin
	  match lr with 
	    | None ->
	      let sep = Indexz.separator Indexz.R z in
	      let index' = index_merge index sep right in
	      let ipos' = add_index slab index' in
	      let z2 = Indexz.suppress Indexz.R ipos' sep_c z in
	      let sep_c = None in (* after the merge, we're clueless *)
	      let index2 = Indexz.close z2 in
	      xxx_merged slab sep_c index2 rest
	    | Some s ->
	      let index' = index_merge index s right in
	      let ipos' = add_index slab index' in
	      let z2 = Indexz.suppress Indexz.R ipos' lr z in
	      let sep_c = None in
	      let index2 = Indexz.close z2 in
	      xxx_merged slab  sep_c index2 rest
	end
      in
      match index, rest with
	| (_,[]) , []  -> ()
	| index , [] -> let _ = add_index slab index in ()
	| index , Index_down z :: rest when index_below_min d index -> 
	  begin
	    let nb = Indexz.neighbours z in
	    match nb with
	      | Indexz.NL pos ->  
		begin
		  let left = read_index pos in
		  if index_mergeable d left 
		  then merge_left left index z rest
		  else (* borrow from left *)
		    let psep = Indexz.separator Indexz.L z in
		    let left',sep',right' = index_borrow_left left psep index in
		    let lpos' = add_index slab left' in
		    let rpos' = add_index slab right' in
		    xxx_borrowed_left slab lpos' sep' rpos' z sep_c rest
		end
		  
	      | Indexz.NR pos ->  
		begin
		  let right = read_index pos in
		  if index_mergeable d right 
		  then merge_right right index z sep_c rest
		  else
		    let sep = match sep_c with
		      | None -> Indexz.separator Indexz.R z 
		      | Some s -> s
		    in
		    let left', right' = index_borrow_right index (Some sep) right in
		    let lpos = add_index slab left' in
		    let rpos = add_index slab right' in
		    let lr' = index_min_key right in
		    xxx_borrowed_right slab lpos rpos z lr' rest
		end
		
	      | Indexz.N2 (pl,pr) -> 
		let left = read_index pl in
		let right = read_index pr in
		match (index_mergeable d left,index_mergeable d right) with
		  | true,_ -> merge_left left index z rest
		  | _, true -> merge_right right index z sep_c rest
		  | _,_ -> (* be consistent with leaf strategy: borrow from left *)
		    begin
		      let psep = Indexz.separator Indexz.L z in
		      let left', sep', right' = index_borrow_left left psep index in
		      let lpos' = add_index slab left' in
		      let rpos' = add_index slab right' in
		      xxx_borrowed_left slab lpos' sep' rpos' z sep_c rest
		    end

	  end
	| _ -> let ipos = L.add slab (Index index) in
	       delete_rest slab ipos sep_c rest
    in
    let trail = descend (L.root t) [] in
    let start = L.next t in
    let () = delete_start slab start trail 
    in ()


  let delete (t:L.t) k =
    let slab = L.make_slab t in
    let () = _delete t slab k in
    L.write t slab


  let _range t 
      (first: k option) finc 
      (last: k option) linc 
      (max: int option) f = 
    let root = L.root t in
    let t_left k = match first with
      | None -> true
      | Some k_f -> if finc then k_f <= k else k_f < k 
    and t_right k = match last with
      | None -> true
      | Some k_l -> if linc then k <= k_l else k < k_l 
    and ti_left k = match first with
      | None -> true
      | Some k_f -> k_f <= k
    and ti_right k = match last with
      | None -> true
      | Some k_l -> k <= k_l
    and t_max count = match
	max with
	  | None -> true
	  | Some m -> count < m
    in
    let rec walk count pos = 
      let e = L.read t pos in
      match e with
	| NIL     -> count
	| Value _ -> count
	| Leaf leaf -> walk_leaf count leaf
	| Index index -> walk_index count index
	| Commit pos -> walk count pos
    and walk_leaf count leaf = 
      let rec loop count = function
	| [] -> count
	| (k,_) :: t -> 
	  if t_max count 
	  then 
	    begin 
	      if t_left k 
	      then 
		let () = f k in
		if t_right k 
		then loop (count + 1) t
		else count
	      else
		count
	    end
	  else count
      in
      loop count leaf
    and walk_index count (p,kps) = 
      let rec loop count p  = function
	| [] -> walk count p
	| (k,pk) :: t when ti_left k -> 
	  begin
	    if t_max count 
	    then let count' = walk count p in
		 loop count' pk t
	    else count
	  end
	| (k,pk) :: t -> 
	  begin
	    if ti_right k 
	    then 
	      let count' = walk count p in
	      if ti_right k && t_max count' 
	      then loop count' pk t 
	      else count'
	    else
	      count
	  end
      in
      loop count p kps
    in
    walk 0 root
  

  let range (t:L.t) (first:k option) (finc:bool) (last:k option) (linc:bool) (max:int option) = 
    let acc = ref [] in
    let f k = acc := k :: !acc in
    let _ = _range t first finc last linc max f in
    List.rev !acc
    
end 

