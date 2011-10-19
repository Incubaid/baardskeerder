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

(* .. *)

let d = 2

type pos = int
type k = string

exception NOT_FOUND of k

type v = string

type kp = k * pos

let kpl2s l = 
  let b = Buffer.create 128 in
  let add_s s = Buffer.add_string b s in
  let add_p p = Buffer.add_string b (Printf.sprintf "%i" p) in
  let pair k p = add_s (Printf.sprintf "%S" k); add_s ", "; add_p p in
  let rec loop = function
    | [] -> ()
    | [k,p] -> pair k p
    | (k,p) :: t -> pair k p; add_s "; "; loop t
  in
  add_s "[";
  loop l ;
  add_s "]";
  Buffer.contents b


