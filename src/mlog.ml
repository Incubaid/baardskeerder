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


type s = { mutable es: entry array;
           mutable next: int;
         }
type t = { spindles: s array;
           mutable current_spindle: int;
           mutable now: Time.t;
         }

type 'a m = 'a
let bind a f = f a
let return v = v
let run a = a

let _d = ref 2 

let init ?(d=2) _ _ = _d := d

let get_d (_:t) = !_d

let now t = t.now

let sync (_:t)  = ()

let close (_:t) = ()

let make_spindles n p = Array.init n (fun _ -> { es=Array.make p NIL; next=0 })

let make2 ?(n_spindles = 4) (_:string) now = { spindles=make_spindles n_spindles 32; current_spindle=0; now }
let make (s:string) = make2 s Time.zero

let write (t:t) (slab:Slab.t) =
  let sp = Spindle t.current_spindle
  and s = Array.get t.spindles t.current_spindle in

  let off = s.next in
  let externalize_pos = function
    | (Outer _) as p -> p
    | Inner i -> Outer (sp, (Offset (i + off)))
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
    let pos = externalize_pos (Commit.get_pos c) in
    let actions = externalize_actions (Commit.get_actions c) in
    let time = Commit.get_time c in
    let previous = externalize_pos (Commit.get_previous c) in
    let lookup = externalize_pos (Commit.get_lookup c) in
    Commit.make_commit ~pos ~previous ~lookup time actions
  in
  let externalize = function
    | NIL -> NIL
    | (Value _) as e -> e
    | Leaf l -> Leaf (externalize_leaf l)
    | Index i -> Index (externalize_index i)
    | Commit c -> Commit (externalize_commit c)
  in

  let do_one _ e =
    s.es.(s.next) <- (externalize e);
    s.next <- s.next + 1
  in
  let current = Array.length s.es in
  let needed = s.next + Slab.length slab in
  if needed > current
  then
    begin
      let new_size = max (current * 2) needed in
      let bigger = Array.make new_size NIL in
      Array.blit s.es 0 bigger 0 current;
      s.es <- bigger
    end;
  Slab.iteri slab do_one ;
  t.now <- Slab.time slab;
  t.current_spindle <- ((t.current_spindle + 1) mod (Array.length t.spindles))
    
let last t =
  let i =
    let j = t.current_spindle - 1 in
      if j < 0
      then j + Array.length t.spindles
      else j
  in
  let s = Array.get t.spindles i in
  Outer (Pos.Spindle i, Offset (s.next - 1))


            
let size (_:entry) = 1

let read t = function
  | Outer (Spindle s, Offset o) ->
      if o < 0
        then NIL
        else Array.get (Array.get t.spindles s).es o
  | Inner _ -> failwith "can't read inner"


let lookup (t:t) = 
  let (p0:pos) = last t in
  match p0 with
    | Inner _ -> failwith "can't do inner"
    | p0 -> bind (read t p0) 
      (function 
        | Commit c -> Commit.get_lookup c
        | e -> failwith "no commit"
      )

let dump ?out:(o=stdout) (t:t) =
  Printf.fprintf o "Next = %d %d\n" t.current_spindle
    (Array.get t.spindles t.current_spindle).next;

  Array.iteri
    (fun i s ->
      Printf.fprintf o "Spindle %2i\n" i;
      Printf.fprintf o "----------\n";

      Array.iteri
        (fun i' a ->
          let s' = Entry.entry2s a in
          Printf.fprintf o "%2i: %s\n" i' s')
      s.es)
    t.spindles

let clear (t:t) =
  Array.iteri
    (fun i s ->
      s.next <- 0;
      Array.fill s.es 0 (Array.length s.es) NIL)
  t.spindles;
  t.current_spindle <- 0

let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
  ignore min_blocks;
  ignore progress_cb;
  failwith "todo"
