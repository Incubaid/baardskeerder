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

open Entry
open Pos

type t = { mutable es: entry list; mutable nes: int}

let make () = { es = []; nes = 0}

let string_of_slab s = Printf.sprintf "{ es = %s; nes = %i}" 
  (Pretty.string_of_list entry2s s.es) s.nes
  
let add slab e =
  slab.es <- e :: slab.es;
  let c = slab.nes in
  slab.nes <- c + 1;
  Inner c
    
let length slab = slab.nes
  
let rev_es t= List.rev t.es

let iter f slab = List.iter f (List.rev slab.es)

