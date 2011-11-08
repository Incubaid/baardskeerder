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

type t = { mutable es : entry array; 
	   mutable next:int}
type slab = {mutable _es: entry list; mutable pos : int}

let make ?(cap=32) () = {es = Array.make cap NIL; next = 0}

let make_slab t = {_es=[]; pos = t.next}

let add slab e = 
  slab._es <- e :: slab._es;
  let c = slab.pos in
  slab.pos <- c + 1; 
  c


let write t slab = 
  let do_one e = 
    t.es.(t.next) <- e;
    t.next <- t.next + 1
  in
  let current = Array.length t.es in
  let needed = t.next + List.length slab._es in
  if needed > current
  then
    begin
      let new_size = max (current * 2) needed in
      let bigger = Array.make new_size NIL in
      Array.blit t.es 0 bigger 0 current;
      t.es <- bigger
    end;
  List.iter do_one (List.rev slab._es)
    
let last t = t.next -1
let next t = t.next
let size (_:entry) = 1
let read t pos = if pos < 0 then NIL else t.es.(pos)

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

let string_of_slab s = Pretty.string_of_list entry2s s._es
