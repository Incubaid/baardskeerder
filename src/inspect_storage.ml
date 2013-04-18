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
      PNode of 'a * (char * 'a prefix_tree) list ref

  let walk pt f =
    let rec loop pt pre depth =
      let pre' = String.create (depth + 1) in
      let () = String.blit pre 0 pre' 0 depth in
      match pt with
        | PNode (s, reflist) ->
            List.iter
              (fun (c, pt) ->
                pre'.[depth] <- c;
                let () = f pt pre' depth in
                loop pt pre' (depth + 1)) !reflist in
    loop pt "" 0

  let get_value = function
    | PNode (value, htbl) -> !value

  let print pt to_string =
    walk pt (fun pt pre depth -> print_endline (pre ^ " : " ^ (to_string (get_value pt))))


  let add current_prefix pt_zipper k apply_v default =
    let current_prefix_length = String.length current_prefix
    and key_length = String.length k in
    let common_prefix_length =
      try
        let rec inner i = if current_prefix.[i] = k.[i] then inner (i + 1) else i in
        inner 0
      with Invalid_argument _ -> min current_prefix_length key_length in
    let rec ntl l n =
      if n = 0 then l else (ntl (List.tl l) (n - 1)) in
    let current_pt_zipper = ntl pt_zipper (current_prefix_length - common_prefix_length) in
    let rec add_nodes pt_zipper offset = match pt_zipper with
      | [] -> failwith "wrong invocation"
      | PNode (_, listref)::_ ->
          if offset < key_length
          then
            let pt' = PNode(default (), ref []) in
            listref := (k.[offset], pt') :: !listref;
            add_nodes (pt' :: pt_zipper)  (offset + 1)
          else
            pt_zipper in
    let pt_zipper' = add_nodes current_pt_zipper common_prefix_length in
    List.iter (fun (PNode(s, _)) -> apply_v s) pt_zipper';
    (pt_zipper', k)


  let inspect_storage t =
    let f (pt_zipper, current_prefix) k o =
      L.read t o >>= (fun entry -> match entry with
        | Entry.Value v ->
            let acc = add
              current_prefix
              pt_zipper
              k
              (fun s -> let vs, va = !s in s := (vs + String.length v , va + 1))
              (fun () -> ref (0, 0)) in
            return (true, acc)
        | _ -> failwith "no value!")
    in
    DBL._fold_reverse_range_while t None false None false f ([PNode (ref (0,0), ref [])], "") >>= fun (ptz, _) ->
    let rec last = function
      | [] -> failwith "empty list"
      | [i] -> i
      | hd::tl -> last tl in
    return (last ptz)


  let _print_bucket b =
    let (pre, vs, va, w, _) = b in
    Printf.printf "%s\t%i\t%i\t%i\n" pre vs va w

  let bucketize pt weight n =
    let to_list (PNode (_, listref)) pre =
      List.map
        (fun (c, (PNode (s, _) as pt')) ->
          let vs, va = !s in
          (pre ^ Char.escaped c, vs, va, weight vs va, pt'))
        !listref in
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
    print_endline "Prefix\t\tTotal size\tNumber of values\tTotal estimated size";
    List.iter _print_bucket sorted

end
