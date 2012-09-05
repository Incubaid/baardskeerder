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

open Log
open Pos
open Entry
open Base
open Dbx

module Rewrite
  (S0:functor(ST0:Bs_internal.STORE) -> LOG with type 'a m = 'a ST0.m)
  (S1:functor(ST1:Bs_internal.STORE) -> LOG with type 'a m = 'a ST1.m) =
  functor (IO:Bs_internal.STORE) ->
  struct

  type update = { mutable kvs: (k * v) list; mutable w: int} 

  module L0 = S0(IO)
  module L1 = S1(IO)

  module DBX1 = DBX(L1)

  let (>>=) = IO.bind
  and return = IO.return
  module M = Monad.Monad(IO)
    
  let max_w = 1024 * 1024

  let make_update () = {kvs = []; w = 0}

  let add_kvs u ((k,v) as kv) = 
    u.kvs <- kv :: u.kvs ;
    u.w <- u.w + (String.length k) + (String.length v)
      
  let update_w u = u.w
    
  let rewrite (l0:L0.t) (root0:pos) (l1:L1.t) = 

    let apply_update u is_last = 
      let inc = if is_last 
        then Time.last_minor
        else Time.next_minor
      in
      DBX1.with_tx l1 ~inc
	    (fun tx -> 
          M.iter (fun (k,v) -> DBX1.set tx k v) u.kvs  
          >>= fun () -> return (OK ())
        )
    in
    let read_value pos = 
      L0.read l0 pos >>= function
	    | Value v -> return v
	    | NIL | Leaf _ | Commit _ | Index _ -> failwith "value!"
    in
    let fat u = update_w u  > max_w in
    let rec walk acc pos =
      L0.read l0 pos >>= function
	    | NIL -> return acc
	    | Value _ -> return acc
	    | Leaf leaf -> walk_leaf acc leaf
	    | Index index -> walk_index acc index
	    | Commit c -> let pos = Commit.get_pos c in walk acc pos
    and walk_leaf acc leaf = 
      let rec loop acc = function
	    | [] -> return acc
	    | (k,pos)::t -> read_value pos >>= fun v ->
		  let () = add_kvs acc (k,v) in
		  loop acc t
      in
      loop acc leaf >>= fun acc1 ->
      if fat acc1 
        then 
        begin
          let () = Printf.printf "fat\n%!" in
          apply_update acc1 false >>= function
            | OK () -> return (make_update ())
            | NOK k -> failwith "todo"
        end
        else
          return acc1
    and walk_index acc (p0,kps) = 
      let rec loop acc p = function
	    | [] -> walk acc p
	    | (_,pk) :: t -> walk acc p >>= fun acc' ->
		  loop acc' pk t
      in
      loop acc p0 kps
    in
    let u0 = make_update () in
    walk u0 root0 >>= fun u ->
    apply_update u true
  
  end
