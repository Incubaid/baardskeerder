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

include Base

module Logs =
  struct
    module Flog = Flog.Flog
    module Flog0 = Flog0.Flog0
  end

module Stores =
  struct
    module Memory = Store.Memory
    module Sync = Store.Sync
    module Lwt = Store.Lwt
  end

module Baardskeerder
  (LF: functor(S: Store.STORE) -> Log.LOG with type 'a m = 'a S.m) =
  functor(S: Store.STORE) ->
    struct
      module L = LF(S)
      module D = Tree.DB(L)
      module X = Dbx.DBX(L)

      type t = L.t
      type tx = X.tx

      let init fn = L.init ~d:6 fn Time.zero
      let make = L.make

      let close = L.close

      let with_tx log f = X.with_tx log (fun v -> L.return (f v))

      let get_latest t k = D.get t k
      let get tx k = X.get tx k
      let set tx k v = X.set tx k v
      let delete tx k = X.delete tx k
    end

module SB = Baardskeerder(Logs.Flog0)(Stores.Sync)
include SB
