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

module MyLog = Flog0.Flog0(Store.Sync)
module MyDB = Tree.DB(MyLog)

let main () =
  MyLog.init "test.db" Time.zero;
  let db = MyLog.make "test.db" in

  MyDB.set db "key" "value123";

  let v = MyDB.get db "key" in

  Printf.printf "Value: %s\n" v;

  List.iter
    (fun i -> Unix.unlink (Printf.sprintf "test.db.%d" i))
    [0; 1; 2]

;;

main ()
