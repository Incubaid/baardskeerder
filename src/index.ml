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

let index2s (p0,rest) = 
  let b= Buffer.create 128 in
  Buffer.add_string b (pos2s p0);
  Buffer.add_string b ", ";
  Buffer.add_string b (kpl2s rest);
  Buffer.contents b






let index_merge (pl,kps_left) sep  (p2, kps_right) = 
  let rec check = function
    | [] -> ()
    | [k,_] -> if k = sep then 
	let s = Printf.sprintf "can't merge:(%s) %S (%s)" (index2s (pl,kps_left)) sep (index2s (p2, kps_right)) in
	failwith s
    | _ :: t -> check t
  in 
  let () = check kps_left in 
  let r = (kps_left @ ((sep,p2) :: kps_right)) in
  pl, r

let index_below_min d (_,t) = List.length t < d
let index_mergeable d (_,t)  = List.length t <= d

let index_borrow_left (pl,kpsl) psep (pr,kpsr) = 
  let rec split_last acc = function
    | [] -> failwith "xxx"
    | [x] -> x , (List.rev acc)
    | x :: t -> split_last (x :: acc) t
  in
  let (lmk,lmp), new_kpsl = split_last [] kpsl in
  let new_left = pl, new_kpsl in
  let new_sep = lmk in
  let new_right = lmp, (psep,pr):: kpsr in
  new_left, new_sep, new_right

let index_borrow_right (pl, kps_l) sep_o (pr, kps_r) = 
  
  match kps_r with
    | [] -> failwith "cannot borrow from empty index"
    | (kr0,pr0)::r -> let lrev = List.rev kps_l in
		      let sep = match sep_o with | None -> kr0 | Some sep -> sep in
		      let left' =   pl, List.rev ((sep,pr):: lrev) in
		      let right' =  pr0, r in
		      left', right'


let index_min_key (_,kps) = 
  match kps with
    | [] -> failwith "empty?"
    | (k,_)::_ -> k



