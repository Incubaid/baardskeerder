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

type pos = int
type k = string
type v = string

type kp = k * pos
type leaf = kp list
type leaf_z = leaf * leaf

type index = pos * (kp list)
type index_z = 
  | Top of index
  | Loc of ((pos * (kp list)) * (kp list))

let leaf2s l = 
  let b = Buffer.create 128 in
  let add_s s = Buffer.add_string b s in
  let add_p p = Buffer.add_string b (Printf.sprintf "%i" p) in
  let pair k p = add_s (Printf.sprintf "%S" k); add_s ", "; add_p p in
  let rec loop = function
    | [] -> ()
    | [k,p] -> pair k p
    | (k,p) :: t -> pair k p; add_s "; "; loop t
  in
  add_s "[";
  loop l ;
  add_s "]";
  Buffer.contents b
    

let index2s (p0,rest) = 
  let b= Buffer.create 128 in
  Buffer.add_string b (Printf.sprintf "%i" p0);
  Buffer.add_string b ", ";
  Buffer.add_string b (leaf2s rest);
  Buffer.contents b

let lz2s (c,t) = Printf.sprintf "(%s,%s)" (leaf2s c) (leaf2s t)

let iz2s = function
  | Top (p0, kps) -> Printf.sprintf "Top (%i,%s)" p0 (leaf2s kps)
  | Loc ((p0,c),t) -> Printf.sprintf "Loc ((%i,%s),%s)" p0 (leaf2s c) (leaf2s t)

let leaf_find_delete leaf k = 
  let rec loop z = match z with
    | _, [] -> None
    | _, (k0,_)   :: _    when k < k0 ->  None
    | _, (k0,p0)  :: _    when k = k0 ->  Some (p0, z)
    | c, h :: t -> loop (h::c,t)
  in
  loop ([],leaf)

let index_find_set index k = 
  let rec loop z = 
    match z with
    | Top (_,[]) -> z
    | Top (_ , (k0, _) :: _) when k < k0      -> z
    | Top ((p0, h :: t))                      -> let pre = p0, [h] in
						 let z' = Loc (pre, t) in
						 loop z'

    | Loc (_, [])                             -> z
    | Loc (_ , (ki,pi) :: _) when k < ki      -> z
    | Loc ( (p0,c) , ((ki,pi) as h :: t))     -> let pre  = p0, (h :: c) in
						 let z' = Loc (pre, t) in
						 loop z'
  in loop (Top index)


let make_indexz (p0, kps) = Top ((p0, kps))

let indexz_pos = function
  | Top ( p0,_) -> p0
  | Loc ((_, (k,pi) ::_) , _      )   -> pi



let indexz_replace pos z = 
  match z with
  | Top (p0, kps)                         -> (pos,kps)
  | Loc ((p0, (k,pi) :: t ), []         ) -> (p0, List.rev ((k,pos):: t))
  | Loc ((p0,            c), (k,px) :: t) ->
    let rec loop acc = function
      | [] -> p0, acc
      | h :: t -> loop ( h :: acc) t
    in
    loop ((k,pos)::t) c

let leafz_delete = function
  | c,h::t -> (List.rev c) @ t
  | _ -> failwith "leafz_delete"

let d = 2



let leaf_min t = List.length t = d
let leaf_merge left right = left @ right

let leafz_max (c,t) = List.length c + List.length t = 2 * d - 1
let leafz_min (c,t) = List.length c + List.length t = d

let indexz_max z = 
  let z_size = match z with
    | Top (_,kps) -> List.length kps
    | Loc ((_,c),t) -> List.length c + List.length t 
  in
  z_size = 2  (* in function of d *)
  
let leafz_left (c,t) = 
  match t with 
  | h :: t' -> (h::c, t') 
  | _ -> failwith "left?"

let leafz_right (c,t) = 
  match c with
  | h :: c' -> c', (h:: t)
  | _ -> failwith "right?"


let indexz_right = function
  | Top (p0  ,h :: t)          -> Loc ((p0,[h]),t)
  | Loc ((p0, c), h :: t) -> Loc ((p0, h :: c), t)

let indexz_left = function
  | Loc ((p0, h :: c), t) -> Loc ((p0, c), h::t)


type merger = L | R

let indexz_suppress d pn z = 
  match d with 
    | R ->
      begin
	match z with 
	  | Top (p0, (_,p1)::t)                  -> pn,t
	  | Loc ((p0, (kc,pc) :: c), (kt,pt)::t) -> p0, (List.rev c) @ (kc,pn) :: t
      end
    | L ->
      match z with
	| Loc ((p0, (kc,pc)::c),[]) -> pn, (List.rev c)
	| _ -> failwith "L"


type neigbours = 
  | NR of pos
  | NL of pos
  | N2 of (pos * pos)



let indexz_neighbours = function
  | Top (p0, (k,p1)::t)        -> NR p1
  | Loc ((p0, (kc,pc) ::c), (kt,pt) :: t) -> N2 (pc,pt)
  | Loc ((p0,  (kc,pc)::c), [] ) -> NL p0

let leafz_close (c,t) = (List.rev c) @ t

let leafz_balance ((c,t) as z) = 
  let ls = List.length  c in
  let n,move = 
    if ls > d 
    then
      ls - d, leafz_right
    else
      d - ls, leafz_left
  in
  let rec loop z = function
    | 0 -> z
    | i -> loop (move z) (i-1)
  in
  loop z n


let leafz_split k pos (c,t) = 
  let l,r = leafz_balance (c, (k,pos)::t) in
  let lift = List.hd l in
  List.rev l, lift, r
    
    
let indexz_close = function
  | Top index -> index
  | Loc ((p0,c), t) -> p0, (List.rev c) @ t
    
let indexz_balance z = 
  let ls = match z with
    | Top ((_,c)) -> List.length c 
    | Loc ((_,c), _) -> List.length c
  in 
  let n, move = 
    if ls > d 
    then
      ls - (d -1), indexz_right
    else
      (d -1)- ls, indexz_left
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
  | Loc ((p0,(k,p)::t),[]) -> Loc ( (p0, (sep,rpos):: (k,lpos) :: t), [])
  | z -> raise (IZ z)


let indexz_split lpos sep rpos z = 
  let z1 = indexz_insert lpos sep rpos z in
  let z2 = indexz_balance z1 in
  let r = 
    match z2 with
      | Loc ((p0, (ks,ps) :: c), t) -> 
	let left =  (p0, List.rev c) in
	let right = (ps, t) in
	left, ks, right
  in
  r


let leaf_find_set leaf k = 
  let rec loop z = match z with
    | c, ((k0,p0) as h) :: t when k0 < k -> loop (h::c, t)
    | c, (k0,_ ) :: t when k0 = k -> c,t 
    | z -> z
  in
  loop ([],leaf)

let leafz_insert k p (c,t) = (List.rev c) @ (k,p) :: t



