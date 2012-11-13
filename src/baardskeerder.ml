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
include Pack

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
  (LF: functor(S: Bs_internal.STORE) -> Log.LOG with type 'a m = 'a S.m) =
  functor(S: Bs_internal.STORE) ->
    struct
      module L = LF(S)
      module D = Tree.DB(L)
      module X = Dbx.DBX(L)
      module C = Catchup.Catchup(L)
      module PrL = Prefix.Prefix(L)
      type t = L.t
      type tx = X.tx

      let init fn = L.init ~d:6 fn Time.zero
      let make = L.make

      let close = L.close

      let with_tx log f = X.with_tx log (fun v -> f v)

      let log_update log ?(diff=true) f = X.log_update log ~diff f
      let last_update log = X.last_update log
      let commit_last log = X.commit_last log
      let catchup log i0 f a0 = C.catchup i0 f a0 log

      let get_latest t k = ((D.get t k): v result L.m)

      let key_count_latest t = D.key_count t

      let range_latest t first finc last linc max = 
        D.range t first finc last linc max
      let range_entries_latest t first finc last linc max = 
        D.range_entries t first finc last linc max
      let rev_range_entries_latest t first finc last linc max = 
        D.rev_range_entries t first finc last linc max
      let prefix_keys_latest t prefix max = 
        PrL.prefix_keys_latest t prefix max

      let get tx k = X.get tx k
      let set tx k v = X.set tx k v
      let delete tx k = X.delete tx k
      let delete_prefix tx k = X.delete_prefix tx k

      let range tx first finc last linc max = 
        X.range tx first finc last linc max
      let range_entries tx first finc last linc max = 
        X.range_entries tx first finc last linc max
      let rev_range_entries tx first finc last linc max = 
        X.rev_range_entries tx first finc last linc max

      let prefix_keys tx prefix max = X.prefix_keys tx prefix max
      
      let set_metadata t s = L.set_metadata t s
      let get_metadata t = L.get_metadata t
      let unset_metadata t = L.unset_metadata t 
    end

module SB = Baardskeerder(Logs.Flog0)(Stores.Sync)
include SB
