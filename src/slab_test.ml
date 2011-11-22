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

open OUnit
open Entry
open Slab
open Pos


let es = [| Value "A"; 
	    Leaf[("a",Inner 0)];
	    Value "B";
	    Leaf[("a",Inner 0); ("b",Inner 2)];
	    Value "C";
	    Leaf[("a",Inner 0);("b",Inner 2);("c",Inner 4)];
	    Value "D";
	    Leaf[("a",Inner 0);("b",Inner 2);("c",Inner 4); ("d", Inner 6)];
	   |]


let t_compaction () = 
  let slab = Slab.make () in
  let () = Array.iter (fun e -> ignore(Slab.add slab e)) es in
  let () = Printf.printf "slab:\n%s\n" (Slab.string_of_slab slab) in
  let slab' = Slab.compact slab in
  let () = Printf.printf "slab':\n%s\n" (Slab.string_of_slab slab') in
  ()
(*
  let mark = Slab.mark slab in
  let p_one i a = 
    let c = match a with
      | true  -> ' '
      | false -> 'x' 
    in
    Printf.printf "%c%3i: %s\n" c i (entry2s es.(i))
  in
  let () = Array.iteri p_one mark in
  let () = Printf.printf "---------------\n" in
  let mapping = Slab.mapping mark in
  let () = Hashtbl.iter (fun k v -> Printf.printf "%i => %i\n%!" k v) mapping in
  let () = Printf.printf "---------------\n" in
  let lookup_pos = function
    | Outer x -> Outer x
    | Inner x -> Inner (Hashtbl.find mapping x)
  in
  let rewrite_leaf l = List.map (fun (k,p) -> (k, lookup_pos p)) l in
  let c = ref 0 in
  let p_one i a = 
    if a then 
      begin
	let e = es.(i) in
	let e' = match e with
	  | Leaf l -> Leaf (rewrite_leaf l )
	  | _ -> e
	in
	let () = Printf.printf "%4i: %s\n" !c (entry2s e') in
	incr c
      end
    else
      ()
  in
  Array.iteri p_one mark
*)

let suite = "Slab" >::: [
  "compaction" >:: t_compaction;
]


