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

type f = int * bool

type t = int * int * bool

let get_major (t,_,_) = t
let get_minor (_,f,_) = f

let make x y g = (x,y, g)

let zero :t = (0, 0, false ) 

let next_major (x,_,_) = (x+1, 0,false)
let next_minor (x,y,g) = 
  if g 
  then (x,y+1,true)
  else failwith "next minor?"

let last_minor (x,y,_) = (x, y+1, false)

let same_major (x0,_,_) (x1,_,_) = x0 = x1

let time2s (x,y,g) = Printf.sprintf "(%i, %i, %b)" x y g

let (=>:) (x0,y0,_) (x1,y1,_) = 
  if x0 < x1 
  then false
  else 
    if x0 > x1 
    then true
    else 
      if y0 < y1 
      then false
      else true
        


