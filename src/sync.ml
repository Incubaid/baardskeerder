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
open Monad

module Sync (L:LOG) = struct
  open Commit
  open Entry
  open Time

  let (>>=) = L.bind
  and return = L.return

  module M = Monad(L)

  let fold_actions t0 (f:'a -> Time.t -> caction -> 'a) a0 log =
    let read_commit p =
      L.read log p >>= function
        | Commit c -> return c
        | _ -> failwith "not a commit node"
    in
    let no_prev = Pos.Outer (Pos.Spindle 0, Pos.Offset 0) in
    let rec build ps p =
      read_commit p >>= fun c ->
      let tc = Commit.get_time c in
      if tc =>: t0 then
        let p' = Commit.get_previous c in
        let ps' = p :: ps in
        if p' = no_prev
        then return ps'
        else
          build ps' p'
      else
        return ps
    in
    let p0 = L.last log in
    build [] p0 >>= fun rps ->
    M.fold_left
      (fun acc p ->
        read_commit p >>= fun c ->
        let actions = Commit.get_cactions c in
        let t = Commit.get_time c in
        let f' acc a = f acc t a in
        return (List.fold_left f' acc actions))
      a0 rps

end
