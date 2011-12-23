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

module MyLog = Flog0
module MyDBX = Dbx.DBX(MyLog)
module MyDB = Tree.DB(MyLog)

type t = MyLog.t
type tx = MyDBX.tx

include Base

let init fn = MyLog.init ~d:6 fn Time.zero
let make fn = MyLog.make fn 

let close log = MyLog.close log

let with_tx log f = MyDBX.with_tx log f


let get_latest t k = MyDB.get t k

let get tx k = MyDBX.get tx k
let set tx k v = MyDBX.set tx k v
let delete tx k = MyDBX.delete tx k

