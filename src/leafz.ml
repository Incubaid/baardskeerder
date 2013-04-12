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

open Leaf

type leaf_z = leaf * leaf

let lz2s (c,t) = Printf.sprintf "(%s,%s)" (leaf2s c) (leaf2s t)

let find_delete leaf k =
  let rec loop z = match z with
    | _, [] -> None
    | _, (k0,_)   :: _    when k < k0 ->  None
    | _, (k0,p0)  :: _    when k = k0 ->  Some (p0, z)
    | c, h :: t -> loop (h::c,t)
  in
  loop ([],leaf)

let delete = function
  | c,_:: t -> let leaf = ((List.rev c) @ t) in
               let sep_c =
                 if t = []
                 then
                   match c with
                     | [] -> None
                     | (k,_) :: _ -> Some k
                 else None

               in leaf, sep_c
  | _ -> failwith "leafz_delete"


let max d (c,t) = List.length c + List.length t = 2 * d - 1
let min d (c,t) = List.length c + List.length t = d

let left (c,t) =
  match t with
    | h :: t' -> (h::c, t')
    | _ -> failwith "left?"

let right (c,t) =
  match c with
    | h :: c' -> c', (h:: t)
    | _ -> failwith "right?"

let close (c,t) = (List.rev c) @ t


let balance d ((c,_) as z) =
  let ls = List.length  c in
  let n,move =
    if ls > d
    then
      ls - d, right
    else
      d - ls, left
  in
  let rec loop z = function
    | 0 -> z
    | i -> loop (move z) (i-1)
  in
  loop z n


let split d k pos (c,t) =
  let l,r = balance d (c, (k,pos)::t) in
  let lift = List.hd l in
  List.rev l, lift, r

let insert k p (c,t) = (List.rev c) @ (k,p) :: t
