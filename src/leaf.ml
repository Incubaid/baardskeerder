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


let leaf2s l = kpl2s l



let length t = List.length t
let leaf_min d t = length t = d
let leaf_mergeable d t = length t <= d
  

let leaf_borrow_right left right = match right with
  | [] -> failwith "leaf_borrow_right"
  | (k,_) as h ::t -> let rev = List.rev left in 
	    let left' = List.rev (h :: rev) in
	    left',k, t

let leaf_borrow_left left right = let rev = List.rev left in
  match rev with
    | [] -> failwith "leaf_borrow_left"
    | h0 :: ((k1,_) as h1) :: t -> 
      let right' = h0 :: right in
      let left' = List.rev (h1 :: t) in
      left' , k1, right'

let rec leaf_max_key = function
  | [] -> failwith "empty"
  | [k,_] -> k
  | _ :: t -> leaf_max_key t

let leaf_min_key = function
  | [] -> failwith "empty"
  | (k,_):: _ -> k

let leaf_merge left right = 
  let leaf = left @ right in
  let m = leaf_max_key leaf in
  leaf, Some m


let leaf_find_set leaf k = 
  let rec loop z = match z with
    | c, ((k0,_) as h) :: t when k0 < k -> loop (h::c, t)
    | c, (k0,_ ) :: t when k0 = k -> c,t 
    | z -> z
  in
  loop ([],leaf)


