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

open Entry
open Pos

type t = { mutable es: entry array; mutable nes: int}

let make () = { es = Array.make 32 NIL; 
		nes = 0}

let string_of_slab s = Printf.sprintf "{ es = %s; nes = %i}" 
  (Pretty.string_of_array entry2s s.es) s.nes
  



let add slab e =
  let c = slab.nes in
  let () = 
    if c = Array.length slab.es 
    then 
      slab.es <- Array.init (4*c) (fun i -> if i < c then slab.es.(i) else NIL)
  in
  slab.es.(c) <- e;
  slab.nes <- c + 1;
  Inner c


let add_value slab v = add slab (Value v)
let add_leaf slab kps = add slab (Leaf kps)
let add_index slab index = add slab (Index index)
let add_commit slab p = add slab (Commit p)

let length slab = slab.nes
  
let is_empty slab = slab.nes = 0

let last slab = Inner (slab.nes -1)
let next slab = Inner (slab.nes)

let iteri slab f = 
  let rec loop i = 
    if i = slab.nes 
    then ()
    else let () = f i slab.es.(i) in loop (i+1) 
  in
  loop 0

let iteri_rev slab f =
  let rec loop i = 
    if i < 0 
    then ()
    else let () = f i slab.es.(i) in loop (i-1)
  in
  loop (slab.nes -1)

      
let read slab pos = match pos with
  | Inner x -> slab.es.(x)
  | Outer _ -> failwith "can't read outer"


let dump s = 
  let do_one i e = Printf.printf "%i:%s\n%!" i (entry2s e) in
  iteri s do_one


let mark slab = 
  let r = Array.make (slab.nes) false in
  let maybe_mark = function
    | Outer _ -> ()
    | Inner x -> r.(x) <- true
  in
  let maybe_mark2 (_,p) = maybe_mark p in
  let mark _ e = 
    match e with
      | NIL | Value _ -> ()
      | Commit (p,_) -> maybe_mark p
      | Leaf l -> List.iter maybe_mark2 l
      | Index (p0,kps) -> let () = maybe_mark p0 in List.iter maybe_mark2 kps
  in
  let () = iteri_rev slab mark in
  let () = r.(slab.nes -1) <- true in
  r

let mapping mark = 
  let s = Array.length mark in
  let h = Hashtbl.create s in
  let rec loop i o =
    if i = s then h
    else
      let v = mark.(i) in
      let i' = i + 1 in
      let () = Hashtbl.add h i o in
      let o' = if v then o + 1 else o in
      loop i' o'
  in
  loop 0 0


let compact s = 
  let s_mark = mark s in
  let s_map = mapping s_mark in
  let lookup_pos = function
    | Outer x -> Outer x
    | Inner x -> Inner (Hashtbl.find s_map x)
  in
  let rewrite_actions actions = List.map 
    (function
      | Commit.Set (k,p) -> Commit.Set (k, lookup_pos p)
      | (Commit.Delete _) as d  -> d)
    actions
  in
  let rewrite_leaf kps        = List.map (fun (k,p) -> (k,lookup_pos p)) kps in
  let rewrite_index (p0,kps)  = (lookup_pos p0 , rewrite_leaf kps) in
  let rewrite_commit (p,actions)  = (lookup_pos p, rewrite_actions actions) in
  let esa = s.es in
  let size = s.nes in
  let r = Array.create size NIL in
  let rec loop c i = 
    if i = size 
    then { es = r; nes = c}
    else
      begin
	let i' = i + 1 in
	let a = s_mark.(i) in
	if a then
	  let e = esa.(i) in
	  let e' = match e with
	    | Leaf  l  -> Leaf (rewrite_leaf l) 
	    | Index i  -> Index (rewrite_index i)
	    | Commit c -> Commit (rewrite_commit c)
	    | Value _ 
	    | NIL -> e
	  in
	  let () = r.(c) <- e' in
	  let c' = c + 1 in
	  loop c' i' 
	else
	  loop c i'
      end
  in
  loop 0 0
	
	
        
