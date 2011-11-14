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

module type Element = sig
  type k
  type v
  val create: int -> (k * v)
end

module Make (E:Element) = struct
  type k = E.k
  type v = E.v

  type t = {
    b: (k * v) array;
    s: int;
    mutable w: int;
    mutable f: bool;
  }

  let create s =
    let b = Array.init s E.create in
    { b=b; s=s; w=0; f=false; }

  let add t k v =
    Array.set t.b t.w (k, v);
    t.w <- (t.w + 1) mod t.s;
    if t.w = 0 then t.f <- true else ()

  let get t k =
    let (i, j) = 
      if t.f = false
      then
        (* Loop from 0 to (w - 1) *)
        (0, t.w - 1)
      else
        (* Loop from w to (w - 1) *)
        (t.w, t.w - 1)
    in

    if (i = 0) && (j = 0)
    then None
    else
      if i = j
      then failwith "i = j"
      else
        let rec loop e =
        let (k', v) = Array.get t.b e in
        if k' = k
        then Some v
        else
          let n = (e + 1) mod t.s in
          if n = i
          then None
          else loop n

      in loop i
end

(*
module IntStringBuffer = Make(
  struct
    type k = int
    type v = string
    let create _ = (0, "")
  end)

let test () =
  let b = IntStringBuffer.create 5 in
  IntStringBuffer.add b 0 "0";
  IntStringBuffer.add b 1 "1";
  IntStringBuffer.add b 2 "2";
  IntStringBuffer.add b 3 "3";
  IntStringBuffer.add b 4 "4";
  IntStringBuffer.add b 5 "5";
  IntStringBuffer.add b 6 "6";

  assert (IntStringBuffer.get b 1 = None);
  assert (IntStringBuffer.get b 2 = Some "2");
  assert (IntStringBuffer.get b 7 = None);

  IntStringBuffer.add b 7 "7";
  assert (IntStringBuffer.get b 2 = None)
*)
