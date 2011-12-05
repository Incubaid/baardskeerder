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
open Base
open Slab


type t = { mutable es : entry array; 
	   mutable next:int;
           mutable next_i: int;
         }

let _d = ref 2 

let init ?(d=2) _ = _d := d

let get_d (_:t) = !_d

let get_i t = t.next_i

let sync (_:t)  = ()

let close (_:t) = ()

let make  (_:string) = {es = Array.make 32 NIL; next = 0; next_i = 0}


let write t (slab:Slab.t) = 
  let off = t.next in
  let externalize_pos = function
    | (Outer _) as p -> p
    | Inner i -> Outer (i + off)
  in
  let externalize_actions xs = 
    let externalize_action = function
      | Commit.Set (k,p) -> Commit.Set (k, externalize_pos p) 
      | (Commit.Delete _) as x-> x
    in
    List.fold_left (fun acc a -> externalize_action a :: acc) [] xs
  in
  let externalize_leaf  l = List.map (function (k,p) -> (k,externalize_pos p)) l in
  let externalize_index (p0, l) = (externalize_pos p0, externalize_leaf l) in
  let externalize_commit c = 
    let p = externalize_pos (Commit.get_pos c) in
    let actions = externalize_actions (Commit.get_actions c) in
    let i = Commit.get_i c in
    Commit.make_commit p i actions
  in
  let externalize = function
    | NIL -> NIL
    | (Value _) as e -> e
    | Leaf l -> Leaf (externalize_leaf l)
    | Index i -> Index (externalize_index i)
    | Commit c -> Commit (externalize_commit c)
  in
  let do_one _ e = 
    t.es.(t.next) <- (externalize e);
    t.next <- t.next + 1
  in
  let current = Array.length t.es in
  let needed = t.next + Slab.length slab in
  if needed > current
  then
    begin
      let new_size = max (current * 2) needed in
      let bigger = Array.make new_size NIL in
      Array.blit t.es 0 bigger 0 current;
      t.es <- bigger
    end;
  Slab.iteri slab do_one ;
  t.next_i <- t.next_i + 1
    
let last t = Outer (t.next -1)

let next t = Outer t.next

let size (_:entry) = 1

let read t = function
  | Outer pos -> if pos < 0 then NIL else t.es.(pos)
  | Inner _ -> failwith "can't read inner"


let dump ?out:(o=stdout) (t:t) =
  Printf.fprintf o "Next = %d\n" t.next;

  Array.iteri
    (fun i a ->
      let s = Entry.entry2s a in
      Printf.fprintf o "%2i: %s\n" i s)
    t.es

let clear (t:t) = 
  let rec loop i = 
    if i = t.next then ()
    else let () = t.es.(i) <- NIL in loop (i+1) 
  in
  loop 0;
  t.next <- 0

let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
  ignore min_blocks;
  ignore progress_cb;
  failwith "todo"
