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
open Tree
open Log
open Entry
open Slab
open Commit
open Catchup
open Prefix

module DBX(L:LOG) = struct

  let (>>=) = L.bind
  and return = L.return

  type tx = { log: L.t; slab: Slab.t; 
              mutable cactions: caction list}

  module DBL = DB(L)
  module CaL = Catchup(L)
  module PrL = Prefix(L)

  let get tx k = DBL._get tx.log tx.slab k

  let set tx k v = 
    let vpos = Inner (Slab.length tx.slab) in
    DBL._set tx.log tx.slab k v >>= fun _ ->
    let a = CSet (k, vpos) in
    let () = tx.cactions <- a :: tx.cactions in
    return ()

  let delete tx k = 
    DBL._delete tx.log tx.slab k >>= fun r ->
    let r' = match r with
      | OK _ ->
        let a = CDelete k in
        let () = tx.cactions <- a :: tx.cactions in
        OK ()
      | NOK k -> NOK k
    in
    return r'


  let with_tx ?(inc=Time.next_major) log f = 
    let now = L.now log in
    let fut = inc now in
    let slab = Slab.make fut in
    let tx = {log;slab;cactions = []} in
    f tx >>= fun txr ->
    match txr with
      | OK a ->
        let root = Slab.length tx.slab -1 in
        let previous = L.last log in
        let pos = Inner root in
        let lookup = pos in
        let commit = make_commit ~pos ~previous ~lookup fut (List.rev tx.cactions) false in
        let c = Commit commit in
        let _ = Slab.add tx.slab c in
          (* let slab' = slab in *)
        let slab' = Slab.compact tx.slab in 
        L.write log slab' >>= fun () ->
        return txr
      | NOK k -> return txr 



  let range (tx:tx) (first:k option) (finc:bool) (last:k option) (linc:bool) (max:int option) = 
    DBL.range tx.log first finc last linc max

  let rev_range_entries (tx:tx) (first:k option) (finc: bool) (last: k option) (linc: bool) (max:int option) = 
    DBL.rev_range_entries tx.log first finc last linc max

  let range_entries (tx:tx) (first:k option) (finc: bool) (last: k option) (linc: bool) (max:int option) = 
    DBL.range_entries tx.log first finc last linc max

  let prefix_keys (tx:tx) (prefix : string) (max: int option) =  
    PrL.prefix_keys tx.log tx.slab prefix max

  let multi_delete (tx:tx) (keys: k list) : (int,k) Base.result L.m =
    let rec _inner (acc:int) keys = match keys with
      | [] -> let r = OK acc in
              return r
      | k :: keys -> 
        begin 
          delete tx k >>= function
            | OK r   -> _inner (acc+1) keys
            | NOK k  -> return (NOK k)
        end
    in _inner 0 keys
    
  let delete_prefix (tx:tx) (prefix : k) = 
    let max = Some 256 in
    let rec _inner tx acc : (int,Base.k) result L.m =
      prefix_keys tx prefix max >>= fun keys ->
      match keys with 
        | []   -> return (OK acc)
        | keys -> 
          begin
            multi_delete tx keys >>= fun r ->
            match r with 
              | OK i -> 
                _inner tx (acc + i)
              | r -> return r
          end                  
    in 
    _inner tx 0 >>= function
    | OK i -> return i
    | NOK k -> failwith (Printf.sprintf "delete_prefix: %s" k)

  let log_update (log:L.t) ?(diff = true) (f: tx -> ('a,'b) result L.m) =
    let _find_lookup () = 
      let pp = L.last log in
      L.read log pp >>= function 
        | Commit lc -> 
          let lu = if diff 
            then Commit.get_pos lc 
            else Commit.get_lookup lc
          in return lu
        | NIL -> return pp
        | e -> failwith (Printf.sprintf "log_update: %s is not commit" (entry2s e))
    in
    let now = L.now log in
    let fut = if diff then Time.next_major now else now in

    let slab = Slab.make fut in
    let tx = {log;slab; cactions = []} in
    
    _find_lookup () >>= fun lookup ->
    f tx >>= function 
      | OK x ->
        begin
          let root = Slab.length tx.slab -1 in
          let previous = L.last log in
          let pos = Inner root in
          let commit = make_commit ~pos ~previous ~lookup fut (List.rev tx.cactions) false in
          let c = Commit commit in
          let _ = Slab.add tx.slab c in
          let slab' = Slab.compact tx.slab in
          L.write log slab' >>= fun () ->
          return (OK x)
        end
      | NOK k -> return (NOK k)

  let commit_last (log:L.t) =
    let pp = L.last log in
    (L.read log pp >>= function
      | Commit lc -> L.return lc
      | e -> failwith (Printf.sprintf "_read_commit: %s is not commit" (entry2s e))
    ) >>= fun lc ->
    let time = Commit.get_time lc in
    let slab = Slab.make time in
    let tx = {log;slab;cactions = []} in
    let pos = Commit.get_pos lc in
    let previous = Commit.get_previous lc in
    let lookup = pos in
    let cactions = Commit.get_cactions lc in
    let commit = make_commit ~pos ~previous ~lookup time cactions true in
    let c = Commit commit in
    let _ = Slab.add tx.slab c in
    L.write log slab
    
  let last_update (log:L.t) =
    let pp = L.last log in
    L.read log pp >>= function
      | Commit lc ->
        let cas = Commit.get_cactions lc in
        let time = Commit.get_time lc in
        let i = Time.major_of time in
        let explicit = Commit.is_explicit lc in
        CaL.translate_cactions log cas >>= fun actions ->
        L.return (Some (i, actions, explicit))
      | NIL -> L.return None
      | e -> failwith (Printf.sprintf "last_update: %s should be commit" (entry2s e))
        
end
