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

open OUnit
open Dbx
module MDBX = DBX(Mlog)


let get_after_delete () = 
  let mlog = Mlog.make () in
  let () = MDBX.with_tx mlog (fun tx -> MDBX.set tx "a" "A") in
  let test tx = 
    MDBX.delete tx "a";
    let _ = MDBX.get tx "a" in
    ()
  in
  OUnit.assert_raises (Base.NOT_FOUND "a") (fun () -> MDBX.with_tx mlog test)



let suite = "DBX" >::: ["get_after_delete" >:: get_after_delete];
