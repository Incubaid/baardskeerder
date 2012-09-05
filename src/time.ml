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

type major = int64
type minor = int
type t = major * minor * bool

let time2s (x,y,g) = Printf.sprintf "(%Lu, %i, %b)" x y g

let get_major (t,_,_) = t
let get_minor (_,f,_) = f

let make (x:major) y g = (x,y, g)

let zero :t = ( 0L, 0, false) 

let next_major (x,_,_) = (Int64.succ x, 0,false)
let next_minor (x,y,g) = 
  if g 
  then (x,y+1,true)
  else failwith (Printf.sprintf "next minor for %s" (time2s (x,y,g)))

let last_minor (x,y,_) = (x, y+1, false)

let major_of ((x:major),_,_) = x

let same_major (x0,_,_) (x1,_,_) = x0 = x1



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

let (>:) (x0,y0,_) (x1,y1,_) = 
  if x0 > x1 
  then true
  else 
    if x0 = x1 && y0 > y1 
    then true
    else false
        


