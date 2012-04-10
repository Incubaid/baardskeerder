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

type action = 
  | Set of (k * pos)
  | Delete of k

let action2s = function
  | Set (k,p) -> Printf.sprintf "Set (%S,%s)" k (pos2s p)
  | Delete k  -> Printf.sprintf "Delete %S" k

type commit = { pos: pos ; 
                previous: pos; 
                lookup: pos;
                time:Time.t; 
                actions: action list;}

let make_commit ~pos ~previous ~lookup time actions = {pos;previous; lookup; time;actions }

let get_pos t = t.pos

let get_actions t = t.actions

let get_lookup t = t.lookup

let get_time t = t.time

let get_previous t = t.previous

let commit2s t = Printf.sprintf "{pos=%s; previous = %s; lookup=%s;time=%s; actions=%s}" 
  (pos2s t.pos) (pos2s t.previous)  (pos2s t.lookup)
  (Time.time2s t.time)
  (Pretty.string_of_list action2s t.actions)
