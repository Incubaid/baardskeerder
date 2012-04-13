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

open Base

type caction = 
  | CSet of (k * pos)
  | CDelete of k

let caction2s = function
  | CSet (k,p) -> Printf.sprintf "CSet (%S,%s)" k (pos2s p)
  | CDelete k  -> Printf.sprintf "CDelete %S" k



type commit = { pos: pos ; 
                previous: pos; 
                lookup: pos;
                time:Time.t; 
                cactions: caction list;}

let make_commit ~pos ~previous ~lookup time cactions = {pos;previous; lookup; time;cactions }

let get_pos t = t.pos

let get_cactions t = t.cactions

let get_lookup t = t.lookup

let get_time t = t.time

let get_previous t = t.previous

let commit2s t = Printf.sprintf "{pos=%s; previous = %s; lookup=%s;time=%s; cactions=%s}" 
  (pos2s t.pos) (pos2s t.previous)  (pos2s t.lookup)
  (Time.time2s t.time)
  (Pretty.string_of_list caction2s t.cactions)
