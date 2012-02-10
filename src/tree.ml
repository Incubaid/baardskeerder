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
open Slab

module DB = functor (L:LOG ) -> struct

  let _get (t:L.t) (slab:Slab.t) (k:k) = 
    let _read pos = match pos with
      | Outer _ -> L.read t pos 
      | Inner _ -> Slab.read slab pos
    in
    let rec descend pos = 
      let e = _read pos in
      match e with
	| NIL -> raise (NOT_FOUND k)
	| Value v -> v
	| Leaf l -> descend_leaf l
	| Index i -> descend_index i
	| Commit _ -> let msg = Printf.sprintf "descend reached a second commit %s" (Pos.pos2s pos) in
		      failwith msg
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
    let rec descend_root () = 
      if Slab.is_empty slab 
      then
	let pos = L.last t in
	let e = L.read t pos in
	match e with 
	  | Commit c -> descend (Commit.get_pos c)
	  | NIL -> raise (NOT_FOUND k)
	  | Index _ | Leaf _ | Value _ -> failwith "descend_root does not start at appropriate level"
      else
	let pos = Slab.last slab in
	descend pos
    in
    descend_root ()

  let get t k = 
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    _get t slab k 

  let rec _set_descend (t:L.t) slab (k:k) pos trail = 
    let e = match pos with
      | Inner _ -> Slab.read slab pos
      | Outer _ -> L.read t pos  
    in
    match e with
      | NIL     -> []
      | Value _ -> failwith "value ?"
      | Leaf l  -> descend_leaf k trail l
      | Index i -> descend_index t slab k trail i
      | Commit _-> failwith "commit ?"
  and descend_leaf k trail leaf =
    let z = leaf_find_set leaf k in
    Leaf_down z :: trail
  and descend_index t slab k trail index = 
    let z = Indexz.find_set index k in
    let trail' = Index_down z :: trail in
    let pos' = Indexz.pos z in
    _set_descend t slab k pos' trail'

  let _set_descend_root t slab k = 
    if Slab.is_empty slab
    then	
      let pos = L.last t in
      let e = L.read t pos in
      match e with
	| NIL -> []
	| Commit c -> let pos = Commit.get_pos c in _set_descend t slab k pos [] 
	| Index _ | Value _ | Leaf _ -> 
	  let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in failwith s
    else
      let pos = Slab.last slab in
      _set_descend t slab k pos []  
	
	
  let _set (t:L.t) slab k v = 
    let d = L.get_d t in
    let rec set_start slab start trail = 
      match trail with 
      | [] -> let vpos = Slab.add_value slab v in
	      let lpos = Slab.add_leaf  slab [k,vpos] in
	      lpos
      | Leaf_down z :: rest -> 
	if Leafz.max d z 
	then 
	  let left, (sep,_) , right = Leafz.split d k start z in
	  let _    = Slab.add_value slab v     in
	  let lpos = Slab.add_leaf  slab left  in
	  let rpos = Slab.add_leaf  slab right in
	  set_overflow slab lpos sep rpos rest
	else
	  let l = Leafz.insert k start z in
	  let _    = Slab.add_value slab v in
	  let lpos = Slab.add_leaf  slab l in
	  set_rest slab lpos rest 
      | Index_down _ :: _ -> failwith "trail must start with Leaf_down _"
    and set_rest (slab:Slab.t) start = function
      | [] -> start
      | (Index_down z) :: rest -> 
	let index = Indexz.replace start z in
	let ipos = Slab.add_index slab index    in
	set_rest slab ipos rest
      | Leaf_down _ :: _ -> failwith "rest of trail cannot contain Leaf_down _ "
    and set_overflow (slab:Slab.t) lpos sep rpos trail = 
      match trail with 
      | [] -> Slab.add_index slab  (lpos, [sep,rpos]) 
      | Index_down z :: rest -> 
	if Indexz.max d z 
	then 
	  let left, sep', right  = Indexz.split d lpos sep rpos z in
	  let lpos' = Slab.add_index slab left  in
	  let rpos' = Slab.add_index slab right in
	  set_overflow slab lpos' sep' rpos' rest
	else
	  let z' = Indexz.insert lpos sep rpos z in
	  let i' = Indexz.close z' in
	  let start' = Slab.add_index slab i' in
	  set_rest slab start' rest
      | Leaf_down _ :: _ -> failwith "rest of trail cannot contain Leaf_down _ "
    in
    let trail = _set_descend_root t slab k in
    let next = Slab.next slab in 
    set_start slab next trail

	
  let set (t:L.t) k v =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    let (rp':pos) = _set t slab k v in
    let action = Commit.Set (k,Inner 0) in (* a little knowledge is a dangerous thing *)
    let last = L.last t in
    let commit = Commit.make_commit rp' last fut [action] in (* UGLY *)
    let _ = Slab.add_commit slab commit in
    L.write t slab


  let rec _delete_rest slab start (lr : k option) trail = match trail with
    | [] -> start
    | Index_down z :: rest -> 
      begin	  
	let _step slab lr z rest = 
	  let index = Indexz.replace start z in
	  let ipos = Slab.add_index slab index in
	  _delete_rest slab ipos lr rest
	in
	match lr with
	  | None -> _step slab lr z rest
	  | Some sep ->
	    if Indexz.can_go_right z 
	    then 
	      let z' = Indexz.replace_right sep z in
	      let lr' = None in (* since there is a larger subtree, I don't know the lr anymore *)
	      _step slab lr' z' rest
	    else
	      _step slab lr z rest
      end
    | Leaf_down _ :: _ -> failwith "rest trail cannot contain Leaf_down _ "
  let _delete (t:L.t) slab k = 
    let d = L.get_d t in
    let _read pos = match pos with 
      | Inner _ -> Slab.read slab pos
      | Outer _ -> L.read t pos  
    in
    let rec descend pos trail = 
      let e = _read pos in
      match e with
	| NIL -> failwith "corrupt"
	| Value _ -> trail
	| Leaf l -> descend_leaf trail l
	| Index i -> descend_index trail i
	| Commit _ -> let msg = Printf.sprintf "descend reached a second commit %s" (Pos.pos2s pos) in
		      failwith msg
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
	Slab.add_leaf slab leaf'
      | Leaf_down z :: rest ->
	if Leafz.min d z 
	then leaf_underflow slab z rest
	else 
	  let leaf',lr = Leafz.delete z in
	  let _ = Slab.add_leaf slab leaf' in
	  (* let () = Printf.printf "lpos = %s <-> start = %s\n" (pos2s lpos) (pos2s start) in *)
	  _delete_rest slab start lr rest
      | Index_down _ :: _ -> failwith "trail cannot start with Index_down _"

    and leaf_underflow slab leafz rest = 
      match rest with 
	| [] -> 
	  let leaf',_ = Leafz.delete leafz in
	  let (rp:pos) = Slab.add_leaf slab leaf' in 
	  rp
	| Index_down z :: rest -> 
	  begin
	    let read_leaf pos = 
	      let e = _read pos in
	      match e with
		| Leaf l -> l
		| Index _ | Value _ | Commit _ | NIL -> failwith "should be leaf"
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
		      let hpos = Slab.add_leaf slab h in
		      let z' = Indexz.suppress Indexz.R hpos sep_c z in
		      let sep_c' = if Indexz.can_go_right z' then None else sep_c in
		      let index' = Indexz.close z' in
		      xxx_merged slab hpos sep_c' index' rest
		    end
		  else (* borrow from right *)
		    begin
		      let left, _ = Leafz.delete leafz in
		      let left', _, right' = leaf_borrow_right left right in
		      let sep_c = leaf_min_key right in
		      let lpos = Slab.add_leaf slab left' in
		      let rpos = Slab.add_leaf slab right' in
		      xxx_borrowed_right slab lpos rpos z sep_c rest 
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
		      let (hpos:pos) = Slab.add_leaf slab h in
		      let z' = Indexz.suppress Indexz.L hpos sep_c z in
		      let index' = Indexz.close z' in
		      let (rp:pos) = xxx_merged slab hpos sep_c index' rest in
		      rp
		    end
		  else (* borrow from left *)
		    begin
		      let right, sep_c = Leafz.delete leafz in
		      let left', sep', right' = leaf_borrow_left left right in
		      let lpos = Slab.add_leaf slab left' in
		      let rpos = Slab.add_leaf slab right' in
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
			let hpos = Slab.add_leaf slab h in
			let z' = Indexz.suppress Indexz.L hpos lr z in 
			let index' = Indexz.close z' in
			let lr'  = 
			  if Indexz.can_go_right z 
			  then None
			  else lr
			in
			xxx_merged slab hpos lr' index' rest
		      end
		    | _, true ->
		      begin
			let left,_ = Leafz.delete leafz in
			let h, lr = leaf_merge left right in
			let hpos = Slab.add_leaf slab h in
			let z' = Indexz.suppress Indexz.R hpos lr z in
			let index' = Indexz.close z' in
			let lr' = 
			  if Indexz.can_go_right z' 
			  then None
			  else lr
			in
			xxx_merged slab hpos lr' index' rest 
		      end
		    | _,_ -> (* borrow from left *)
		      begin
			let right, sep_c = Leafz.delete leafz in
			let left', sep', right' = leaf_borrow_left left right in
			let lpos = Slab.add_leaf slab left' in
			let rpos = Slab.add_leaf slab right' in
			xxx_borrowed_left slab lpos sep' rpos z sep_c rest
		      end		    
		end
	  end
	| Leaf_down _ :: _  -> failwith "rest of trail cannot contain Leaf_down"
    and xxx_borrowed_right slab lpos rpos  z (lr:k) rest = 
      let z' = Indexz.borrowed_right lpos lr rpos z in
      let lr' = if Indexz.can_go_right z' then None else Some lr in
      let index' = Indexz.close z' in
      let ipos = Slab.add_index slab index' in
      _delete_rest slab ipos lr' rest
    and xxx_borrowed_left slab (lpos:pos) sep (rpos:pos) z lr rest = 
      match lr with
	| None -> 
	  let z2 = Indexz.borrowed_left lpos sep rpos z in
	  let index' = Indexz.close z2 in
	  let ipos' = Slab.add_index slab index' in
	  _delete_rest slab ipos' None rest
	| Some s ->
	  let z2 = 
	    if Indexz.can_go_right z 
	    then Indexz.replace_right s z 
	    else z
	  in
	  let z3 = Indexz.borrowed_left lpos sep rpos z2 in
	  let lr' = if Indexz.can_go_right z3 then None else lr in
	  let index' = Indexz.close z3 in	  
	  let ipos' = Slab.add_index slab index' in
	  _delete_rest slab ipos' lr' rest
    and xxx_merged slab (start:pos) sep_c (index:Index.index) rest = 
      let read_index pos = 
	let e = _read pos in
	match e with
	  | Index i -> i
	  | Value _ | Leaf _ | Commit _ | NIL -> failwith "should be index"
      in
      let merge_left left index z rest = 
	begin
	  let sep = Indexz.separator Indexz.L z in
	  let index1 = index_merge left sep index in
	  let ipos1 = Slab.add_index slab index1 in
	  let z2 = Indexz.suppress Indexz.L ipos1 sep_c z in
	  let lr' = if Indexz.can_go_right z2 then None else sep_c in
	  let index2 = Indexz.close z2 in
	  xxx_merged slab ipos1 lr' index2 rest
	end
      in
      let merge_right right index z lr rest = 
	begin
	  match lr with 
	    | None ->
	      let sep = Indexz.separator Indexz.R z in
	      let index' = index_merge index sep right in
	      let ipos' = Slab.add_index slab index' in
	      let z2 = Indexz.suppress Indexz.R ipos' sep_c z in
	      let sep_c = None in (* after the merge, we're clueless *)
	      let index2 = Indexz.close z2 in
	      xxx_merged slab ipos' sep_c index2 rest
	    | Some s ->
	      let index' = index_merge index s right in
	      let ipos' = Slab.add_index slab index' in
	      let z2 = Indexz.suppress Indexz.R ipos' lr z in
	      let sep_c = None in
	      let index2 = Indexz.close z2 in
	      xxx_merged slab ipos' sep_c index2 rest
	end
      in
      match index, rest with
	| (_,[]) , []  -> (start:pos)
	| index , [] -> Slab.add_index slab index 
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
		    let lpos' = Slab.add_index slab left' in
		    let rpos' = Slab.add_index slab right' in
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
		    let lpos = Slab.add_index slab left' in
		    let rpos = Slab.add_index slab right' in
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
		      let lpos' = Slab.add_index slab left' in
		      let rpos' = Slab.add_index slab right' in
		      xxx_borrowed_left slab lpos' sep' rpos' z sep_c rest
		    end

	  end
	| _ -> let ipos = Slab.add_index slab index in
	       _delete_rest slab ipos sep_c rest
    in
    let descend_root () = 
      if Slab.is_empty slab then
	let lp = L.last t in
	let e = L.read t lp in
	match e with
	  | NIL -> []
	  | Commit c -> let pos = Commit.get_pos c in descend pos []
	  | Index _ | Leaf _ | Value _  -> 
	    let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in
	    failwith s
      else
	descend (Slab.last slab) []
    in
    let trail = descend_root () in
    let start = Slab.next slab in
    let (rp':pos) = delete_start slab start trail in
    rp'


  let delete (t:L.t) k =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    let (rp':pos) = _delete t slab k in
    let action = Commit.Delete k in
    let last = L.last t in
    let commit = Commit.make_commit rp' last fut [action] in
    let _ = Slab.add_commit slab commit in
    L.write t slab


  let _range t 
      (first: k option) finc 
      (last: k option) linc 
      (max: int option) f = 

    let lp = L.last t in
    let e = L.read t lp in
    match e with 
      | NIL  -> 0
      | Commit c -> 
	begin
          let pos = Commit.get_pos c in
	  let root = pos in
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
	      | Commit c -> let pos = Commit.get_pos c in walk count pos
	  and walk_leaf count leaf = 
	    let rec loop count = function
	      | [] -> count
	      | (k,_) :: t -> 
		if t_max count 
		then 
		  begin 
		    if t_left k 
		    then 
		      if t_right k 
		      then 
			let () = f k in
			loop (count + 1) t
		      else count
		    else
		      loop count t
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
		       if ti_right k 
		       then loop count' pk t
		       else count'
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
	end
      | Index _ | Leaf _ | Value _ -> failwith "not expected entry type"

  let range (t:L.t) (first:k option) (finc:bool) (last:k option) (linc:bool) (max:int option) = 
    let acc = ref [] in
    let f k = acc := k :: !acc in
    let _ = _range t first finc last linc max f in
    List.rev !acc


  let _fold_reverse_range_while t
      (first: k option) finc
      (last: k option) linc
      (f: 'a -> Base.k -> Base.pos -> (bool * 'a))
      (is: 'a) =

    let lp = L.last t in
    let e = L.read t lp in

    match e with
      | NIL  -> is
      | Commit c ->
        begin
          let left_of_range k =
            match last with
              | None -> false
              | Some f -> if linc then k < f else k <= f
          and right_of_range k =
            match first with
              | None -> false
              | Some f -> if finc then k > f else k >= f
          in

          let rec walk s pos: (bool * 'a) =
            match L.read t pos with
              | NIL -> failwith "Tree._fold_reverse_range_while: unexpected entry NIL"
              | Value _ -> failwith "Tree._fold_reverse_range_while: unexpected entry Value"
              | Leaf leaf -> walk_leaf s leaf
              | Index index -> walk_index s index
              | Commit c -> failwith "Tree._fold_reverse_range_while: unexpected entry Commit"
          and walk_leaf s leaf =
            let rec loop s' = function
              | [] -> (true, s')
              | (k, _) :: _ when left_of_range k ->
                  (false, s')
              | (k, _) :: kps when right_of_range k ->
                  loop s' kps
              | (k, p) :: kps ->
                  let (cont, s'') = f s' k p in
                  if cont
                  then
                    loop s'' kps
                  else
                    (false, s'')
            in
            loop s (List.rev leaf)
          and walk_index s (p, kps) =
            let rec loop s' = function
              | [] -> walk s' p
              | (k, p') :: kps when left_of_range k ->
                  (* Need to check one index entry left of the lowest 'valid'
                   * entry, since it might point to some more valid keys *)
                  walk s' p'
              | (k, p') :: kps when right_of_range k ->
                  loop s' kps
              | (k, p') :: kps ->
                  let (cont, s'') = walk s' p' in
                  if cont
                  then
                    loop s'' kps
                  else
                    (false, s'')
            in
            loop s (List.rev kps)
          in

          let (_, r) = walk is (Commit.get_pos c) in
          r
        end
      | Index _ | Leaf _ | Value _ ->
        failwith "Tree._fold_reverse_range_while: unexpected entry type"

  let reverse_range (t:L.t)
      (first:k option) (finc:bool)
      (last:k option) (linc:bool)
      (max:int option) =

    let f (count, acc) k _ = begin
      let count' = count + 1 in

      let cont = match max with
        | None -> true
        | Some c -> count' < c
      in

      (cont, (count', k :: acc))
    end in

    let (_, res) = _fold_reverse_range_while t first finc last linc f (0, []) in
    List.rev res


  let confirm (t:L.t) (s:Slab.t) k v =
    let set_needed =
      try
	let vc = _get t s k  in
	vc <> v
      with NOT_FOUND _ -> true
    in
    if set_needed
    then let _  = _set t s k v in ()
    else ()

  let depth (t:L.t) (slab:Slab.t )= 
    let rec _depth_descend t slab pos c = 
      let e = match pos with
        | Inner _ -> Slab.read slab pos
        | Outer _ -> L.read t pos
      in
      match e with
        | NIL -> c
        | Value _ -> c
        | Leaf l -> 
          begin
            match l with
              | [] -> c
              | (_,p) :: _ -> _depth_descend t slab p (c+1)
          end
        | Index (p0,_) -> _depth_descend t slab p0 (c+1)
        | Commit _ -> failwith "reached a second commit on descend"
    in
    let _depth_descend_root t slab =
      if Slab.is_empty slab
      then
        let pos = L.last t in
        let e = L.read t pos in
        match e with
          | NIL -> 0
          | Commit c -> let pos = Commit.get_pos c in _depth_descend t slab pos 1
          | Index _ | Value _ | Leaf _ -> 
            let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in failwith s
      else
        let pos = Slab.last slab in 
        _depth_descend t slab pos 1
    in
    _depth_descend_root t slab
end 

