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

open OUnit
open Entry
open Pos


let es =
  let pos = Inner 25 in
  let t0 = Time.make 1L 2 false in
  let previous = Outer (0, (-1)) in
  let lookup = pos in
  [|
    Value "xxxxx";
    Leaf ["key_00000000", Inner 0];
    Value "xxxxx";
    Leaf ["key_00000000", Inner 0; "key_00000001", Inner 2];
    Value "xxxxx";
    Leaf ["key_00000000", Inner 0; "key_00000001", Inner 2; "key_00000002", Inner 4];
    Value "xxxxx";
    Leaf ["key_00000000", Inner 0; "key_00000001", Inner 2];
    Leaf ["key_00000002", Inner 4; "key_00000003", Inner 6];
    Index (Inner 7, ["key_00000001", Inner 8]);
    Value "xxxxx";
    Leaf ["key_00000002", Inner 4; "key_00000003", Inner 6; "key_00000004", Inner 10];
    Index (Inner 7, ["key_00000001", Inner 11]);
    Value "xxxxx";
    Leaf ["key_00000002", Inner 4; "key_00000003", Inner 6];
    Leaf ["key_00000004", Inner 10; "key_00000005", Inner 13];
    Index (Inner 7, ["key_00000001", Inner 14; "key_00000003", Inner 15]);
    Value "xxxxx";
    Leaf ["key_00000004", Inner 10; "key_00000005", Inner 13; "key_00000006", Inner 17];
    Index (Inner 7, ["key_00000001", Inner 14; "key_00000003", Inner 18]);
    Value "xxxxx";
    Leaf ["key_00000004", Inner 10; "key_00000005", Inner 13];
    Leaf ["key_00000006", Inner 17; "key_00000007", Inner 20];
    Index (Inner 7, ["key_00000001", Inner 14]);
    Index (Inner 21, ["key_00000005", Inner 22]);
    Index (Inner 23, ["key_00000003", Inner 24]);
    Commit (Commit.make_commit ~pos ~previous ~lookup t0 [] false) (* should be correct values *);
  |]


let t_compaction () =
  let fut = Time.make 1L 2 false in
  let slab = Slab.make fut in
  let () = Array.iter (fun e -> ignore(Slab.add slab e)) es in
  let () = Slab.dump slab in
  let () = print_string "----\n" in
  let slab' = Slab.compact slab in
  let () = Slab.dump slab' in
  let () = print_string "---\n" in
  ()
(*
  let mark = Slab.mark slab in
  let p_one i a =
  let c = match a with
  | true  -> ' '
  | false -> 'x'
  in
  Printf.printf "%c%3i: %s\n" c i (entry2s es.(i))
  in
  let () = Array.iteri p_one mark in
  let () = Printf.printf "---------------\n" in
  let mapping = Slab.mapping mark in
  let () = Hashtbl.iter (fun k v -> Printf.printf "%i => %i\n%!" k v) mapping in
  let () = Printf.printf "---------------\n" in
  let lookup_pos = function
  | Outer x -> Outer x
  | Inner x -> Inner (Hashtbl.find mapping x)
  in
  let rewrite_leaf l = List.map (fun (k,p) -> (k, lookup_pos p)) l in
  let c = ref 0 in
  let p_one i a =
  if a then
  begin
  let e = es.(i) in
  let e' = match e with
  | Leaf l -> Leaf (rewrite_leaf l )
  | _ -> e
  in
  let () = Printf.printf "%4i: %s\n" !c (entry2s e') in
  incr c
  end
  else
  ()
  in
  Array.iteri p_one mark
*)

let suite = "Slab" >::: [
  "compaction" >:: t_compaction;
]
