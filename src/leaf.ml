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
  | (k,_) as h ::t -> (left @ [h]), k, t


let leaf_borrow_left left right =
  let rev = List.rev left in
  match rev with
    | h0 :: ((k1,_) :: t as left'rev) ->
        let right' = h0 :: right in
        let left' = List.rev left'rev in
        left' , k1, right'
    | [] | [_] -> failwith "leaf_borrow_left"


let shared_prefix = function
  | [] -> ""
  | (k0,_) :: t ->
      let shared k0 k1 m =
        let l0 = String.length k0
        and l1 = String.length k1 in
        let l = min m (min l0 l1) in
        let rec loop i =
          if i = l
          then l
          else
            if k0.[i] = k1.[i]
            then loop (i+1)
            else i
        in
        loop 0
      in
      let m0 = String.length k0 in
      let m = List.fold_left (fun m (k,_) -> shared k0 k m) m0 t in
      String.sub k0 0 m


let rec leaf_max_key = function
  | [] -> failwith "empty"
  | [k,_] -> k
  | _ :: t -> leaf_max_key t

let leaf_min_key = function
  | [] -> failwith "empty"
  | (k,_):: _ -> k

let leaf_merge left right =
  let leaf = left @ right in
  let m = leaf_max_key right in
  leaf, Some m


let leaf_find_set leaf k =
  let rec loop z = match z with
    | c, ((k0,_) as h) :: t when k0 < k -> loop (h::c, t)
    | c, (k0,_ ) :: t when k0 = k -> c,t
    | z -> z
  in
  loop ([],leaf)
