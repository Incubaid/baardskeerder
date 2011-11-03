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

type index = pos * (kp list)
type index_z = 
  | Top of index
  | Loc of ((pos * (kp list)) * (kp list))


let index2s (p0,rest) = 
  let b= Buffer.create 128 in
  Buffer.add_string b (Printf.sprintf "%i" p0);
  Buffer.add_string b ", ";
  Buffer.add_string b (kpl2s rest);
  Buffer.contents b



let iz2s = function
  | Top (p0, kps) -> Printf.sprintf "Top (%i,%s)" p0 (kpl2s kps)
  | Loc ((p0,c),t) -> Printf.sprintf "Loc ((%i,%s),%s)" p0 (kpl2s c) (kpl2s t)


let index_find_set index k = 
  let rec loop z = 
    match z with
    | Top (_,[]) -> z
    | Top (_ , (k0, _) :: _) when k <= k0     -> z
    | Top ((p0, h :: t))                      -> let pre = p0, [h] in
						 let z' = Loc (pre, t) in
						 loop z'

    | Loc (_, [])                             -> z
    | Loc (_ , (ki,pi) :: _) when k <= ki     -> z
    | Loc ( (p0,c) , ((ki,pi) as h :: t))     -> let pre  = p0, (h :: c) in
						 let z' = Loc (pre, t) in
						 loop z'
  in loop (Top index)


let index_merge (pl,kps_left) sep  (p2, kps_right) = 
  let rec check = function
    | [] -> ()
    | [k,s] -> if k = sep then 
	let s = Printf.sprintf "can't merge:(%s) %S (%s)" (index2s (pl,kps_left)) sep (index2s (p2, kps_right)) in
	failwith s
    | h :: t -> check t
  in 
  let () = check kps_left in 
  let r = (kps_left @ ((sep,p2) :: kps_right)) in
  pl, r

let index_below_min d (p0,t) = List.length t < d
let index_mergeable d (_,t)  = List.length t <= d

let index_borrow_left left right = 
  failwith (Printf.sprintf "index_borrowed_left %s %s" (index2s left) (index2s right))

let index_borrow_right (pl, kps_l) sep_o (pr, kps_r) = 
  
  match kps_r with
    | [] -> failwith "cannot borrow from empty index"
    | (kr0,pr0)::r -> let lrev = List.rev kps_l in
		      let sep = match sep_o with | None -> kr0 | Some sep -> sep in
		      let left' =   pl, List.rev ((sep,pr):: lrev) in
		      let right' =  pr0, r in
		      left', right'


let index_max_key (_,kps) = 
  let rec loop = function
    | [] -> failwith "empty?"
    | [(k,_)] -> k
    | h :: t -> loop t 
  in loop kps

let index_min_key (_,kps) = 
  match kps with
    | [] -> failwith "empty?"
    | (k,_)::_ -> k

let make_indexz (p0, kps) = Top ((p0, kps))


      

  

let indexz_pos = function
  | Top ( p0,_) -> p0
  | Loc ((_, (k,pi) ::_) , _      )   -> pi

let indexz_replace pos z = 
  match z with
  | Top (p0, kps)                -> (pos,kps)
  | Loc ((p0, (k,pi) :: c ), t ) -> (p0, (List.rev ((k,pos) :: c)) @ t)

let indexz_replace_with_sep sep_c pos z =
  match z with
    | Top (p0, ((k,p) :: r))          -> 
      let index = pos,((sep_c,p) :: r) in
      index, None
    | Loc ((p0, (k,pi) ::c), [] ) -> 
      let index = (p0, (List.rev ((k,pos) :: c))) in
      index, Some sep_c
    | Loc ((p0, (k,pi) :: c), (kr,pr)::t) ->
      let left = List.rev ((k,pos)::c) in
      let right = (sep_c,pr) :: t in
      let index = p0, left @ right in
      index, None

let indexz_max d z = 
  let z_size = match z with
    | Top (_,kps) -> List.length kps
    | Loc ((_,c),t) -> List.length c + List.length t 
  in
  z_size = d  
  
let indexz_borrowed_right lpos sep rpos = function
  | Top (p0, (k0,p1)::t) -> Top (lpos,(sep,rpos) ::t)

let indexz_borrowed_left lpos sep rpos = function
  | Loc((p0, [k0,p1]),t)                -> Loc ((lpos, [sep,rpos]),t)
  | Loc((p0, (kx,px):: (ky,py) :: c),t) -> Loc ((p0,(sep,rpos)::(ky,lpos)::c), t)
  | z -> let s= Printf.sprintf "indexz_borrowed_left %i %s %i z=%s\n%!" lpos sep rpos (iz2s z) in failwith s

let indexz_right = function
  | Top (p0  ,h :: t)          -> Loc ((p0,[h]),t)
  | Loc ((p0, c), h :: t)      -> Loc ((p0, h :: c), t)
  | z -> let s = Printf.sprintf "cannot go right: %s\n" (iz2s z) in failwith s

let indexz_left = function
  | Loc ((p0, h :: c), t) -> Loc ((p0, c), h::t)
  | z -> let s = Printf.sprintf "cannot go left: %s\n" (iz2s z) in failwith s



type merger = L | R

let indexz_separator d z = 
  match d with
    | L -> 
      begin
	match z with 
	  | Loc ((_,(kc,_)::c),_) -> kc
      end
    | R ->
      begin
	match z with
	  | Loc ( (_,_), (kt,_)::_) -> kt
	  | Top (p0, (k0,p1):: t)   -> k0
	  | _ -> let s = Printf.sprintf "indexz_separator R (%s)\n" (iz2s z) in failwith s
      end
      
let indexz_suppress d pn sep_o z = 
  let maybe_replace_sep sep sep_o = 
    match sep_o with 
      | None -> sep
      | Some sep -> sep
  in
  match d with 
    | R ->
      begin
	match z with 
	  | Top (p0, (_,p1)::t)                  -> pn,t
	  | Loc ((p0, (kc,pc) :: c), (kt,pt)::t) -> p0, (List.rev c) @ (kc,pn) :: t
      end
    | L ->
      match z with
	| Loc ((p0,[k0, p1]),[])             -> pn,[]
	| Loc ((p0,[_]), (kx,px)::t)          -> 
	  let new_sep = maybe_replace_sep kx sep_o in
	  pn, ((new_sep,px)::t)
	| Loc ((p0, (kl,pl)::(kr,pr)::c),[]) -> p0, (List.rev ((kr,pn)::c)) 
	| Loc ((p0, (kl,pl)::(kr,pr)::c),(kx,px):: t)  -> 
	  let new_sep = maybe_replace_sep kx sep_o in
	  p0, (List.rev ((kr,pn):: c)) @ ((new_sep,px)::t)
	| _ -> let s = Printf.sprintf "suppress L %i z=%s" pn (iz2s z) in failwith s

let indexz_delete index = failwith "todo"


type neigbours = 
  | NR of pos
  | NL of pos
  | N2 of (pos * pos)



let indexz_neighbours = function
  | Top (_, (_,p1) :: _)        -> NR p1
  | Loc ((p0, [_]), [])  -> NL p0
  | Loc ((p0, [_]), (_,pr)::t)  -> N2 (p0,pr)
  | Loc ((p0, (kr,pr) :: (kl,pl) ::c), [] ) -> NL pl
  | Loc ((_, _ :: (_,pl) ::_), (_,pr):: t) -> N2(pl,pr)
  | z -> let s = Printf.sprintf "index_neighbours %s\n" (iz2s z) in failwith s
    
let indexz_close = function
  | Top index -> index
  | Loc ((p0,c), t) -> p0, (List.rev c) @ t
    
let indexz_balance d z = 
  let move, n = match z with
    | Top ((_,c))    -> indexz_right, d
    | Loc ((_,c), r) ->
      let cs = List.length c 
      and rs = List.length r 
      in 
      if cs > rs 
      then indexz_left, (cs - d)
      else indexz_right,  (d -1 - rs)
  in
  let rec loop z = function
    | 0 -> z
    | i -> loop (move z) (i-1)
  in
  loop z n
    
exception IZ of index_z

let indexz_insert lpos sep rpos z = 
  match z with
  | Top ((p0,t)) -> Top (lpos, ((sep,rpos) :: t))
  | Loc ((p0,(k,p)::c),t) -> Loc ((p0, (sep,rpos):: (k,lpos) :: c), t)
  | z -> let s = Printf.sprintf "indexz_insert %i %s %i %s" lpos sep rpos (iz2s z) in failwith s


let indexz_split d lpos sep rpos z = 
  let z1 = indexz_insert lpos sep rpos z in
  let z2 = indexz_balance d z1 in 
  let r = 
    match z2 with
      | Loc ((p0, (k,p)::c), t) ->
	let left = p0, List.rev c in
	let right = p, t in
	left, k,right
      | z -> let s = Printf.sprintf "indexz_split %i %s %i %s=> %s \n" lpos sep rpos (iz2s z) (iz2s z2) in
	     failwith s
  in
  r


