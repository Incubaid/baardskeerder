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
open Index

type index_z = 
  | Top of index
  | Loc of ((pos * (kp list)) * (kp list))

let iz2s = function
  | Top (p0, kps) -> Printf.sprintf "Top (%s,%s)" (pos2s p0) (kpl2s kps)
  | Loc ((p0,c),t) -> Printf.sprintf "Loc ((%s,%s),%s)" (pos2s p0) (kpl2s c) (kpl2s t)

let make_indexz (p0, kps) = Top ((p0, kps))

let find_set index k = 
  let rec loop z = 
    match z with
    | Top (_,[]) -> z
    | Top (_ , (k0, _) :: _) when k <= k0 -> z
    | Top ((p0, h :: t)) -> 
      let pre = p0, [h] in
      let z' = Loc (pre, t) in
      loop z'

    | Loc (_, [])  -> z
    | Loc (_ , (ki,_) :: _) when k <= ki -> z
    | Loc ( (p0,c) , ((_,_) as h :: t))  -> 
      let pre  = p0, (h :: c) in
      let z' = Loc (pre, t) in
      loop z'
  in loop (Top index)


let pos = function
  | Top ( p0,_) -> p0
  | Loc ((_, (_,pi) ::_) , _  ) -> pi
  | Loc ((_,[]),_) -> failwith "illegal Loc"

let replace pos z = 
  match z with
  | Top (_, kps)                -> (pos,kps)
  | Loc ((p0, (k,_) :: c ), t ) -> (p0, (List.rev ((k,pos) :: c)) @ t)
  | Loc ((_,[]),_) -> failwith "illegal Loc"


let max d z = 
  let z_size = match z with
    | Top (_,kps) -> List.length kps
    | Loc ((_,c),t) -> List.length c + List.length t 
  in
  z_size = 2 * d - 2
  
let borrowed_right lpos sep rpos = function
  | Top (_, _::t) -> Top (lpos,(sep,rpos) ::t)
  | Top (_,[]) -> failwith "illegal Top"
  | Loc (_,_) -> failwith "illegal Loc"

let borrowed_left lpos sep rpos = function
  | Loc((_, [_]),t)                -> Loc ((lpos, [sep,rpos]),t)
  | Loc((p0, _ :: (ky,_) :: c),t) -> Loc ((p0,(sep,rpos)::(ky,lpos)::c), t)
  | Top (_,_) 
  | Loc ((_,[]),_) as z -> 
    let s= Printf.sprintf "indexz_borrowed_left %s %s %s z=%s\n%!" 
      (pos2s lpos) sep (pos2s rpos) (iz2s z) in 
    failwith s

let can_go_right = function
  | Top (_, _ :: _) -> true
  | Loc ((_,_), _:: _) -> true
  | Top (_,[]) -> false
  | Loc ((_,_),[]) -> false

let replace_right new_sep = function
  | Top (p0,(_,p1)::t)      -> Top (p0, (new_sep,p1)::t)
  | Loc ((p0,c), (_,pr)::t) -> Loc ((p0,c), (new_sep,pr) :: t)
  | Top (_,[]) 
  | Loc ((_,_),[]) -> failwith "cannot replace right"


let indexz_right = function
  | Top (p0  ,h :: t)          -> Loc ((p0,[h]),t)
  | Loc ((p0, c), h :: t)      -> Loc ((p0, h :: c), t)
  | Top (_,[]) 
  | Loc ((_,_),[]) as z -> let s = Printf.sprintf "cannot go right: %s\n" (iz2s z) in failwith s 

let indexz_left = function
  | Loc ((p0, h :: c), t) -> Loc ((p0, c), h::t)
  | Top _
  | Loc ((_,[]),_) as z -> let s = Printf.sprintf "cannot go left: %s\n" (iz2s z) in failwith s


type merger = L | R

let separator d z = 
  match d with
    | L -> 
      begin
	match z with 
	  | Loc ((_,(kc,_)::_),_) -> kc
	  | Top _ -> failwith "no left"
	  | Loc ((_,[]),_) -> failwith "illegal Loc"
      end
    | R ->
      begin
	match z with
	  | Loc ( (_,_), (kt,_)::_) -> kt
	  | Top (_, (k0,_):: _)   -> k0
	  | Top (_,[]) 
	  | Loc ((_,_),[]) -> let s = Printf.sprintf "indexz_separator R (%s)\n" (iz2s z) in failwith s
      end
      
let suppress d pn sep_o z = 
  let maybe_replace_sep sep sep_o = 
    match sep_o with 
      | None -> sep
      | Some sep -> sep
  in
  match d with 
    | R ->
      begin
	match z with 
	  | Top (_, _::t)                  -> Top (pn,t)
	  | Loc ((p0, (kc,_) :: c), _::t) -> Loc ((p0, (kc,pn):: c) , t)
	  | Top (_,[])     
	  | Loc ((_,_),_) -> failwith "cannot suppress"
      end
    | L ->
      match z with
	| Loc ((_,[_]),[])              -> Top (pn,[])
	| Loc ((_,[_]), (kx,px)::t)     -> 
	  let new_sep = maybe_replace_sep kx sep_o in
	  Top (pn, ((new_sep,px)::t))
	| Loc ((p0, _::(kr,_)::c),[])  -> Loc ((p0, (kr,pn)::c), []) 
	| Loc ((p0, _ :: (kr,_)::c),(kx,px):: t)  -> 
	  let new_sep = maybe_replace_sep kx sep_o in
	  Loc ((p0, (kr,pn)::c), (new_sep,px) :: t)
	| Top _ | Loc ((_,[]),_) -> 
	  let s = Printf.sprintf "suppress L %s z=%s" (pos2s pn) (iz2s z) in failwith s 


type neighbours = 
  | NR of pos
  | NL of pos
  | N2 of (pos * pos)



let neighbours = function
  | Top (_, (_,p1) :: _)        -> NR p1
  | Loc ((p0, [_]), [])  -> NL p0
  | Loc ((p0, [_]), (_,pr)::_)  -> N2 (p0,pr)
  | Loc ((_, _ :: (_,pl) ::_), [] ) -> NL pl
  | Loc ((_, _ :: (_,pl) ::_), (_,pr):: _) -> N2(pl,pr)
  | Top (_,[])  | Loc ((_,[]),_) as z -> 
    let s = Printf.sprintf "index_neighbours %s\n" (iz2s z) in failwith s 
    
let close = function
  | Top index -> index
  | Loc ((p0,c), t) -> p0, (List.rev c) @ t
    
let balance d z = 
  let move, n = match z with
    | Top (_,_)    -> indexz_right, d
    | Loc ((_,c), r) ->
      let cs = List.length c 
      and rs = List.length r 
      in 
      if cs > rs 
      then indexz_left, (cs - d)
      else indexz_right,  (d - cs)
  in
  let rec loop z = function
    | 0 -> z
    | i -> loop (move z) (i-1)
  in
  loop z n
    
exception IZ of index_z

let insert lpos sep rpos z = 
  match z with
  | Top ((_,t)) -> Top (lpos, ((sep,rpos) :: t))
  | Loc ((p0,(k,_)::c),t) -> Loc ((p0, (sep,rpos):: (k,lpos) :: c), t)
  | Loc ((_,[]),_) -> failwith "illegal loc"
  (* | z -> let s = Printf.sprintf "indexz_insert %i %s %i %s" lpos sep rpos (iz2s z) in failwith s *)


let split d lpos sep rpos z = 
  let z1 = insert lpos sep rpos z in
  let z2 = balance d z1 in 
  let r = 
    match z2 with
      | Loc ((p0, (k,p)::c), t) ->
	let left = p0, List.rev c in
	let right = p, t in
	left, k,right
      | Loc ((_,[]),_) -> failwith "illegal loc"
      | Top _  as z -> 
	let s = Printf.sprintf "indexz_split %s %s %s %s=> %s \n" 
	  (pos2s lpos) sep (pos2s rpos) (iz2s z) (iz2s z2) 
	in
	failwith s
  in
  r



