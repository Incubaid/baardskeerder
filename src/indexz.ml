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


type index_z = pos * (kp list) * (kp list)

let iz2s (p0, c, t) = Printf.sprintf "(%s,%s,%s)" (pos2s p0) (kpl2s c) (kpl2s t)

let make_indexz (p0, kps) = (p0, [], kps)

let find_set index k =
  let rec loop z =
    match z with
      | (_, _, []) -> z
      | (_, _, (ki, _) :: _) when k <= ki -> z
      | (p0, c, h :: t) ->
          let z' = (p0, h :: c, t) in
          loop z'
  in loop (make_indexz index)


let pos = function
  | (p0, [], _) -> p0
  | (_, (_, pi) :: _, _) -> pi

let replace pos z =
  match z with
    | (_, [], kps) -> (pos, kps)
    | (p0, (k, _) :: c, t) -> (p0, List.rev_append c ((k,pos) :: t))


let max d z =
  let z_size = match z with
    | (_, c, t) -> List.length c + List.length t
  in
  z_size = 2 * d - 2

let borrowed_right lpos sep rpos = function
  | (_, [], _::t) -> (lpos, [], (sep, rpos)::t)
  | (_, _, _) -> failwith "illegal borrowed_right"


let borrowed_left lpos sep rpos = function
  | (_, [_], t) -> (lpos, [sep, rpos], t)
  | (p0, _ :: (ky,_) :: c, t) -> (p0, (sep,rpos)::(ky,lpos)::c, t)
  | (_, [], _) as z ->
      let s= Printf.sprintf "indexz_borrowed_left %s %s %s z=%s\n%!"
        (pos2s lpos) sep (pos2s rpos) (iz2s z) in
      failwith s

let can_go_right = function
  | (_, _, _ :: _) -> true
  | (_, _, []) -> false

let replace_right new_sep = function
  | (p0, c, (_, pr) :: t) -> (p0, c, (new_sep, pr) :: t)
  | (_, _, []) -> failwith "cannot replace right"


let indexz_right = function
  | (p0, c, h :: t) -> (p0, h :: c, t)
  | (_, _, []) as z -> let s = Printf.sprintf "cannot go right: %s\n" (iz2s z) in failwith s

let indexz_left = function
  | (p0, h :: c, t) -> (p0, c, h :: t)
  | (_, [], _) as z -> let s = Printf.sprintf "cannot go left: %s\n" (iz2s z) in failwith s


type merger = L | R

let separator d z =
  match d with
    | L ->
        begin
          match z with
            | (_, (kc, _)::_, _) -> kc
            | (_, [], _) -> failwith "no left"
        end
    | R ->
        begin
          match z with
            | (_, _, (kt, _) :: _) -> kt
            | (_, _, []) -> failwith "no right"
        end

let suppress d pn sep_o z =
  match d with
    | R ->
        begin
          match z with
            | (_, [], _ :: t) -> (pn, [], t)
            | (p0, (kc, _) :: c, _ :: t) -> (p0, (kc, pn)::c, t)
            | (_, _, _) -> failwith "cannot suppress"
        end
    | L ->
        let new_t = function
          | [] -> []
          | (kx, px) :: t ->
              let new_sep = match sep_o with
                | None -> kx
                | Some sep -> sep in
              (new_sep, px) :: t in
        match z with
          | (_, [_], t) ->                (pn, [], new_t t)
          | (p0, _ :: (kr, _) :: c, t) -> (p0, (kr, pn) :: c, new_t t)
          | (_, [], _) ->
              let s = Printf.sprintf "suppress L %s z=%s" (pos2s pn) (iz2s z) in
              failwith s

type neighbours =
  | NR of pos
  | NL of pos
  | N2 of (pos * pos)



let neighbours = function
  | (_, [], (_, p1) :: _) -> NR p1
  | (p0, [_], []) -> NL p0
  | (p0, [_], (_, pr) :: _) -> N2 (p0, pr)
  | (_, _ :: (_, pl) :: _, []) -> NL pl
  | (_, _ :: (_, pl) :: _, (_, pr) :: _) -> N2 (pl, pr)
  | (_, [], _) as z ->
      let s = Printf.sprintf "index_neighbours %s\n" (iz2s z) in
      failwith s

let close = function
  | (p0, c, t) -> p0, List.rev_append c t

let balance d z =
  let move, n = match z with
    | (_, c, r) ->
        let cs = List.length c
        and rs = List.length r
        in
        if cs > rs
        then indexz_left, (cs - d)
        else indexz_right,  (d - cs)
  in
  let rec loop z = function
    | 0 -> z
    | i -> loop (move z) (i-1)
  in
  loop z n

exception IZ of index_z

let insert lpos sep rpos z =
  match z with
    | (_, [], t) -> (lpos, [], (sep, rpos) :: t)
    | (p0, (k, _) :: c, t) -> (p0, (sep, rpos) :: (k, lpos) :: c, t)


let split d lpos sep rpos z =
  let z1 = insert lpos sep rpos z in
  let z2 = balance d z1 in
  let r =
    match z2 with
      | (p0, (k, p) :: c, t) ->
          let left = p0, List.rev c in
          let right = p, t in
          left, k,right
      | (_, _, _) as z ->
          let s = Printf.sprintf "indexz_split %s %s %s %s=> %s \n"
            (pos2s lpos) sep (pos2s rpos) (iz2s z) (iz2s z2)
          in
          failwith s
  in
  r
