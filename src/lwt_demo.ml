(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2012 Incubaid BVBA
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

open Lwt

module MyLog = Flog0.Flog0(Store.Lwt)
module MyDB = Tree.DB(MyLog)

let main () =
  MyLog.init "test_lwt.db" Time.zero >>= fun () ->
  MyLog.make "test_lwt.db" >>= fun db ->
  MyDB.set db "abc" "123" >>= fun () ->
  MyDB.get db "abc" >>= fun v ->
  Lwt_io.printf "Value: %s\n" v >>= fun () ->
  MyLog.sync db >>= fun () ->
  MyLog.close db >>= fun () ->
  Lwt_list.iter_s
    (fun i -> Lwt_unix.unlink (Printf.sprintf "test_lwt.db.%d" i))
    [0; 1; 2]
;;

Lwt_main.run (main ())
