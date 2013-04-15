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

open Log
open Tree

module Inspect_storage
  (S0:functor(ST:Bs_internal.STORE) -> LOG with type 'a m = 'a ST.m) =
  functor (IO:Bs_internal.STORE) ->
struct

  module L = S0(IO)
  module DBL = DB(L)

  let (>>=) = IO.bind
  and return = IO.return

  type 'a prefix_tree =
      PNode of 'a * (char, 'a prefix_tree) Hashtbl.t

  let walk pt f =
    let rec loop pt pre depth =
      let pre' = String.create (depth + 1) in
      let () = String.blit pre 0 pre' 0 depth in
      match pt with
        | PNode (s, htbl) ->
            Hashtbl.iter
              (fun c pt ->
                pre'.[depth] <- c;
                let () = f pt pre' depth in
                loop pt pre' (depth + 1)) htbl in
    loop pt "" 0

  let get_value = function
    | PNode (value, htbl) -> !value

  let print pt to_string =
    walk pt (fun pt pre depth -> print_endline (pre ^ " : " ^ (to_string (get_value pt))))

  let find_prefix ?f:(f = fun a -> ()) pt k =
    let length = String.length k in
    let rec loop pt offset =
      if offset = length
      then (pt, offset)
      else
        match pt with
          | PNode (s, htbl) ->
              f s;
              let c = k.[offset] in
              try
                let pt' = Hashtbl.find htbl c in
                loop pt' (offset + 1)
              with Not_found -> (pt, offset) in
    loop pt 0

  let add pt default k f =
    let (PNode (s, htbl) as p), offset = find_prefix ~f pt k in
    let length = String.length k in
    if offset < length
    then
      let rec add_nodes (PNode (s, htbl)) k offset =
        if offset < length then
          let c = k.[offset] in
          let s = default () in
          let pt = PNode (s, Hashtbl.create 10) in
          Hashtbl.add htbl c pt;
          add_nodes pt k (offset + 1)
        else
          f s
      in
      add_nodes p k offset
    else
      f s

  let inspect_storage t =
    let f acc k o =
      L.read t o >>= (fun entry -> match entry with
        | Entry.Value v ->
            add
              acc
              (fun () -> (ref (0,0)))
              k
              (fun s ->
                let vs, va = !s in
                s:= (vs + (String.length v), va + 1));
            return (true, acc)
        | _ -> failwith "no value!")
    in
    DBL._fold_reverse_range_while t None false None false f (PNode (ref (0,0), Hashtbl.create 10)) >>= fun (pt) ->
    return pt

  let _print_bucket b =
    let (pre, vs, va, w, _) = b in
    Printf.printf "%s\t%i\t%i\n" pre vs va

  let bucketize pt weight n =
    let to_list (PNode (_, htbl)) pre =
      Hashtbl.fold
        (fun c (PNode (s, _) as pt') acc ->
          let vs, va = !s in
          (pre ^ Char.escaped c, vs, va, weight vs va, pt') :: acc)
        htbl
        [] in
    let rec inner buckets count =
      if count >= n
      then
        buckets
      else
        let compare a b =
          let (_, _, _, w1, _), (_, _, _, w2, _) = (a, b) in
          w1 > w2 in
        let get_heaviest l = List.fold_left (fun a b -> if compare a b then a else b) (List.hd l) (List.tl l) in
        let (pre, _, _, _, pt) as heaviest = get_heaviest buckets in
        let heaviest_buckets = to_list pt pre  in
        inner (List.rev_append heaviest_buckets (List.filter (fun b -> b != heaviest) buckets)) (count - 1 + List.length heaviest_buckets) in
    let buckets = to_list pt "" in
    inner buckets (List.length buckets)


  let print_buckets buckets =
    let sorted = List.sort (fun b1 b2 -> let (pre1, _, _, _, _), (pre2, _, _, _, _) = (b1, b2) in String.compare pre1 pre2) buckets in
    print_endline "prefix\tTotal size\tNumber of values";
    List.iter _print_bucket sorted

end
