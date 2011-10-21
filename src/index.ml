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

let index_below_min (p0,t) = List.length t < d

let indexz_max z = 
  let z_size = match z with
    | Top (_,kps) -> List.length kps
    | Loc ((_,c),t) -> List.length c + List.length t 
  in
  z_size = 2  (* in function of d *)
  
let indexz_borrowed_right lpos sep rpos = function
  | Top (p0, (k0,p1)::t) -> Top (lpos,(sep,rpos) ::t)

let indexz_borrowed_left lpos sep rpos = function
  | Loc((p0, [k0,p1]),[]) -> Loc ((lpos, [sep,rpos]),[])
  | z -> let () = Printf.printf "z=%s\n%!" (iz2s z) in failwith "indexz_borrowed_right"

let indexz_right = function
  | Top (p0  ,h :: t)          -> Loc ((p0,[h]),t)
  | Loc ((p0, c), h :: t)      -> Loc ((p0, h :: c), t)
  | z -> let s = Printf.sprintf "cannot go right: %s\n" (iz2s z) in failwith s

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
	| Loc ((p0,[k0, p1]),[]) -> pn,[]
	| Loc ((p0, (kl,pl)::(kr,pr)::c),[]) -> p0, (List.rev ((kr,pn):: c))
	| _ -> let s = Printf.sprintf "L: z=%s\n" (iz2s z) in failwith s


type neigbours = 
  | NR of pos
  | NL of pos
  | N2 of (pos * pos)



let indexz_neighbours = function
  | Top (p0, (k,p1)::t)        -> NR p1
  | Loc ((p0, [kc,pc]),[])  -> NL p0
  | Loc ((p0, (kc,pc) ::c), (kt,pt) :: t) -> N2 (pc,pt)
  | Loc ((p0, (kr,pr) :: (kl,pl) ::c), [] ) -> NL pl
  | z -> Printf.printf "z=%s\n" (iz2s z); failwith "??"

    
let indexz_close = function
  | Top index -> index
  | Loc ((p0,c), t) -> p0, (List.rev c) @ t
    
let indexz_balance z = 
  let ls,rs = match z with
    | Top ((_,c))    -> 0 , List.length c
    | Loc ((_,c), r) -> List.length c, List.length r
  in 
  let n, move = 
    if ls > rs
    then
      ls - rs - 1, indexz_left
    else
      rs - ls -1 , indexz_right
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
      | Loc ((p0, c), (ks,ps):: t) -> 
	let left =  (p0, List.rev c) in
	let right = (ps, t) in
	left, ks, right
  in
  r

let string_of_kp_list =
  Pretty.string_of_list (Pretty.string_of_pair Pretty.id string_of_int)

let string_of_index = Pretty.string_of_pair string_of_int string_of_kp_list

let string_of_index_z = function
  | Top i -> Printf.sprintf "Top %s"
      (Pretty.string_of_pair string_of_int string_of_kp_list i)
  | Loc ((p, kp1), kp2) ->
      Printf.sprintf "Loc ((%d, %s), %s)"
        p (string_of_kp_list kp1) (string_of_kp_list kp2)
