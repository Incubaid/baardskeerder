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

let d = 2

type k = string
type v = string
type pos = int

type kps = (k * pos) list
type l_o_r = L | R (* siblings *)

type dir = 
  | INSERT
  | REPLACE of (int * pos)


type entry = 
  | NIL
  | Index of (pos * kps)
  | Leaf  of kps
  | Value of v
      
let kps2s (kps:kps) = 
  let b = Buffer.create 128 in
  Buffer.add_string b "[";
  let rec inner = function
    | [] -> ()
    | [(k,p)] -> 
      Buffer.add_string b (Printf.sprintf "(%s,%i)" k p)
    | (k,p) :: t -> 
      Buffer.add_string b (Printf.sprintf "(%s,%i);" k p );
      inner t
  in
  inner kps;
  Buffer.add_string b "]";
  Buffer.contents b


let leaf_dir (k:k) (kps: kps) =
  let rec loop i = function
    | [] -> INSERT
    | (k0,p0) :: _ when k0 = k -> REPLACE (i,p0)
    | _ :: rest -> loop (i+1) rest
  in
  loop 0 kps

let leaf_size (kps:kps) = 1

let index_size (pp, kps) = 1

let leaf_remove i (kps : kps) = 
  let rec loop acc i = function
    | [] -> List.rev acc
    | h :: tail -> 
      let acc' = if i = 0 then acc else h :: acc in
      loop acc' (i-1) tail
  in
  loop [] i kps

let index_replace i (lpos:pos) pp (kps:kps) = 
  if i = 0 then
    (lpos, kps)
  else
    let rec loop acc i = function
      | [] -> (pp,List.rev acc)
      | (kh,ph) as h :: tail ->
	let kp' = 
	  if i = 1 
	  then (kh,lpos) 
	  else h 
	in
	loop (kp' :: acc) (i-1) tail
    in
    loop [] i kps

    
let leaf_overflow kps = List.length kps > (2 * d - 1)

let index_overflow (pp,kps) = List.length kps = (2 * d - 1)
let leaf_underflow kps = List.length kps < d
  
let leaf_split kps = 
  let rec loop acc i = function 
    | [] -> failwith "can't happen"
    | (k,p) as h :: tail ->
      if i = 0 
      then List.rev (h :: acc), k, tail
      else loop (h::acc) (i-1) tail
  in
  loop [] (d-1) kps

let leaf_merge lr kps0 kps1 = 
  match lr with
    | R -> kps0 @ kps1
    | L -> kps1 @ kps0
    
let leaf_replace i p (kps:kps) = 
  let rec loop acc i = function
    | [] -> List.rev acc
    | (k,_) as kp :: tail ->
      let kp' = if i = 0 then (k,p) else kp in
      loop (kp' :: acc) (i-1) tail
  in
  loop [] i kps

let leaf_insert k p (kps:kps) = 
  let rec loop acc = function
    | [] -> List.rev ((k,p) :: acc)
    | (kh,ph) as kph :: tail when kh < k-> loop (kph ::acc) tail
    | tail -> (List.rev ((k,p):: acc)) @ tail
  in
  loop [] kps
    


let index_dir k pp kps = 
  let rec loop i pp  = function
    | [] -> (i,pp)
    | (k0,p0) :: tail ->
      if k <= k0 
      then (i,pp) 
      else loop (i+1) p0 tail
  in
  loop 0 pp kps 
    
let index_insert i0 lpos sep rpos pp kps =
  match kps with
    | [k0,r0] -> 
      if sep > k0 
      then pp, [k0,lpos;sep, rpos]
      else failwith "todo"
      
let index_remove i0 lpos pp kps = 
  match kps with
    | [(k0,p0); (k1,p1)] -> 
      if i0 = 0 
      then lpos, [k1,p1]
      else 
	if i0 = 1 
	then pp,[k0,lpos] 
	else failwith "todo"
	  
	  
	  
	  

let index_n_seps pp kps = List.length kps
  
let pick_sibling i0 me pp kps = 
  let rec loop i pp kps = 
    match kps with
      | [] -> pp,R
      | [(k,p)]  -> 
	if i0 = 0 then p,R 
	else pp,L
      | (k,p) :: t -> 
	if i > 0 then loop (i-1) p t
	else 
	  if pp = me 
	  then p,R
	  else pp,L
	  
	
  in loop i0 pp kps
  
