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

module Sync (L:LOG) = struct
  open Commit
  open Entry
  open Time

  let fold_actions t0 (f:'a -> action -> 'a) a0 log =
    let read_commit p = 
      let e = L.read log p in
      match e with
        | Commit c -> c
        | _ -> failwith "not a commit node"
    in
    let rec build ps p =
      let c = read_commit p in
      let tc = Commit.get_time c in
      if tc =>: t0  then 
        let p' = Commit.get_previous c in
        build (p:: ps) p'
      else
        ps 
    in
    let p0 = L.last log in
    let rps = build [] p0 in
    List.fold_left 
      (fun acc p -> 
        let c = read_commit p in
        let actions = Commit.get_actions c in
        List.fold_left f acc actions) 
      a0 rps
                      
end
