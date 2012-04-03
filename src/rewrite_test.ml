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

open Flog0
open Rewrite
open Tree
open OUnit

module MMRewrite = Rewrite (Flog0) (Flog0) (Store.Memory)
module LLog = Flog0(Store.Memory)
module MDB = DB(LLog)

module M = Monad.Monad(Store.Memory)

let test_presence () =
  let (>>=) = Store.Memory.bind
  and return = Store.Memory.return in 

  let kvs = 
    let rec loop acc = function 
      | 26 -> acc
      | i -> 
	let c0 = Char.chr (65 + i) in
	let c1 = Char.chr (96 + i) in
	let kv = (Printf.sprintf "%c" c0, Printf.sprintf "%c" c1) in
	     loop (kv :: acc) (i+1)
    in
    loop [] 0
  in
  LLog.init "m0" Time.zero >>= fun () ->
  LLog.make "m0" >>= fun m0 ->
  M.iter (fun (k,v) -> MDB.set m0 k v) kvs >>= fun () ->
  let m0t = LLog.now m0 in
  let root0 = LLog.last m0 in
  LLog.init "m1" m0t >>= fun () ->
  LLog.make "m1" >>= fun m1 ->
  MMRewrite.rewrite m0 root0 m1 >>= fun () ->
  let now = LLog.now m0 in
  let fut = Time.next_major now in
  let empty = Slab.make fut in 
  M.iter
    (fun (k,v) -> MDB._get m1 empty k >>= fun v' -> return (OUnit.assert_equal v' v))
    kvs >>= fun () ->
  let m1t = LLog.now m1 in
  let s = Printf.sprintf "%s <> %s" (Time.time2s m0t) (Time.time2s m1t) in
  OUnit.assert_bool s (Time.same_major m0t m1t);
  return ()


let suite = "Rewrite" >::: [
  "presence" >:: fun () -> Store.Memory.run (test_presence ())
]
