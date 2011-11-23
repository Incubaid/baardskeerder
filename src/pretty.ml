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

let string_of_list f l = Printf.sprintf "[%s]"
  (String.concat "; " (List.map f l))

let string_of_array f a = 
  let b = Buffer.create 128 in
  let s = Array.length a in
  Buffer.add_string b "[|";
  Array.iteri (fun i e -> 
    Buffer.add_string b (f e);
    if i < s-1 then Buffer.add_char b ';'
  ) a;
  Buffer.add_string b "|]";
  Buffer.contents b

let id x = x
