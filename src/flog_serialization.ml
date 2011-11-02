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

let ord = Char.code
and uchr = Char.unsafe_chr
and uget = String.unsafe_get
and uset = String.unsafe_set

let check l s o =
  if not (o >= 0 && o <= String.length s - l)
  then invalid_arg "Flog_serialization.check: offset"
  else ()


let write_uint8 i s o = String.set s o (Char.chr i)
and read_uint8 s o = Char.code (String.get s o)


let write_uint32 i s o =
  assert (i >= 0 && i < 0x100000000);
  check 4 s o;

  uset s (o + 0) (uchr ((i lsr  0) land 0xFF));
  uset s (o + 1) (uchr ((i lsr  8) land 0xFF));
  uset s (o + 2) (uchr ((i lsr 16) land 0xFF));
  uset s (o + 3) (uchr ((i lsr 24) land 0xFF))

and read_uint32 s o =
  check 4 s o;

  let i0 = ord (uget s (o + 0))
  and i1 = ord (uget s (o + 1))
  and i2 = ord (uget s (o + 2))
  and i3 = ord (uget s (o + 3)) in

  i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24)


let write_uint64 i s o =
  assert (i >= 0 && i <= 0X3FFFFFFFFFFFFFFF); (* max_int *)
  check 8 s o;

  uset s (o + 0) (uchr ((i lsr  0) land 0xFF));
  uset s (o + 1) (uchr ((i lsr  8) land 0xFF));
  uset s (o + 2) (uchr ((i lsr 16) land 0xFF));
  uset s (o + 3) (uchr ((i lsr 24) land 0xFF));
  uset s (o + 4) (uchr ((i lsr 32) land 0xFF));
  uset s (o + 5) (uchr ((i lsr 40) land 0xFF));
  uset s (o + 6) (uchr ((i lsr 48) land 0xFF));
  uset s (o + 7) (uchr ((i lsr 56) land 0xFF))

and read_uint64 s o =
  check 8 s o;

  let i0 = ord (uget s (o + 0))
  and i1 = ord (uget s (o + 1))
  and i2 = ord (uget s (o + 2))
  and i3 = ord (uget s (o + 3))
  and i4 = ord (uget s (o + 4))
  and i5 = ord (uget s (o + 5))
  and i6 = ord (uget s (o + 6))
  and i7 = ord (uget s (o + 7)) in

  i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24) + (i4 lsl 32) + (i5 lsl 40)
    + (i6 lsl 48) + (i7 lsl 56)

