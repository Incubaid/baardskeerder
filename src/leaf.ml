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

type leaf = kp list
type leaf_z = leaf * leaf

let leaf2s l = kpl2s l

let lz2s (c,t) = Printf.sprintf "(%s,%s)" (leaf2s c) (leaf2s t)

let leaf_find_delete leaf k = 
  let rec loop z = match z with
    | _, [] -> None
    | _, (k0,_)   :: _    when k < k0 ->  None
    | _, (k0,p0)  :: _    when k = k0 ->  Some (p0, z)
    | c, h :: t -> loop (h::c,t)
  in
  loop ([],leaf)

let leaf_min t = List.length t = d
let leaf_merge left right = left @ right

let leafz_delete = function
  | c,h::t -> (List.rev c) @ t
  | _ -> failwith "leafz_delete"

let leafz_max (c,t) = List.length c + List.length t = 2 * d - 1
let leafz_min (c,t) = List.length c + List.length t = d


let leafz_left (c,t) = 
  match t with 
  | h :: t' -> (h::c, t') 
  | _ -> failwith "left?"

let leafz_right (c,t) = 
  match c with
  | h :: c' -> c', (h:: t)
  | _ -> failwith "right?"

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


let leaf_find_set leaf k = 
  let rec loop z = match z with
    | c, ((k0,p0) as h) :: t when k0 < k -> loop (h::c, t)
    | c, (k0,_ ) :: t when k0 = k -> c,t 
    | z -> z
  in
  loop ([],leaf)

let leafz_insert k p (c,t) = (List.rev c) @ (k,p) :: t

let string_of_leaf =
    Pretty.string_of_list (Pretty.string_of_pair Pretty.id string_of_int)

let string_of_leaf_z = Pretty.string_of_pair string_of_leaf string_of_leaf
