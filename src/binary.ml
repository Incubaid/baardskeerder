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

type 'a writer = Buffer.t -> 'a -> unit
type 'a reader = string -> int -> ('a * int)

let (>>) (f: 'a writer) (g: 'a writer): 'a writer = fun b a ->
  f b a;
  g b a

let (>>=) (f: 'a reader) (g: 'a -> 'b reader): 'b reader = fun b o ->
  let (v, o') = f b o in
  g v b o'

let return (a: 'a): 'a reader = fun _ o ->
  (a, o)

let run_writer ?buffer_size:(bs=64): ('a writer) -> 'a -> (string * int) =
  fun w -> fun a ->
    let b = Buffer.create bs in
    let () = w b a in
    (Buffer.contents b, Buffer.length b)

let ord = Char.code
and uchr = Char.unsafe_chr
and uget = String.unsafe_get
and uset = String.unsafe_set

let check l s o =
  if not (o >= 0 && o <= String.length s - l)
  then invalid_arg "Binary.check: offset"
  else ()

let size_uint8 = 1
and write_uint8: ('a -> int) -> 'a writer =
  fun f -> fun b a -> Buffer.add_char b (Char.chr (f a))
and read_uint8: int reader =
  fun s o -> (Char.code (String.get s o), succ o)

let size_char8 = 1
and write_char8: ('a -> char) -> 'a writer =
  fun f -> fun b a -> Buffer.add_char b (f a)
and read_char8: char reader =
  fun s o -> (String.get s o, succ o)

let write_uint32_to_buffer: Buffer.t -> int -> unit =
  fun b v ->
    if (v < 0 || v >= 0x100000000)
    then invalid_arg "Binary.write_uint32_to_buffer: out of range"
    else ();

    Buffer.add_char b (uchr ((v lsr  0) land 0xFF));
    Buffer.add_char b (uchr ((v lsr  8) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 16) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 24) land 0xFF))

let size_uint32 = 4
and write_uint32: ('a -> int) -> 'a writer =
  fun f -> fun b a ->
    let v = f a in
    write_uint32_to_buffer b v
and read_uint32: int reader =
  fun s o ->
    check 4 s o;

    let i0 = ord (uget s (o + 0))
    and i1 = ord (uget s (o + 1))
    and i2 = ord (uget s (o + 2))
    and i3 = ord (uget s (o + 3)) in

    (i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24), o + 4)

let size_uint64 = 8
and write_uint64: ('a -> int) -> 'a writer =
  fun f -> fun b a ->
    let v = f a in

    if (v < 0 || v >= 0X3FFFFFFFFFFFFFFF)
    then invalid_arg "Binary.write_uint64: out of range"
    else ();

    Buffer.add_char b (uchr ((v lsr  0) land 0xFF));
    Buffer.add_char b (uchr ((v lsr  8) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 16) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 24) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 32) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 40) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 48) land 0xFF));
    Buffer.add_char b (uchr ((v lsr 56) land 0xFF))
and read_uint64: int reader =
  fun s o ->
    check 8 s o;

    let i0 = ord (uget s (o + 0))
    and i1 = ord (uget s (o + 1))
    and i2 = ord (uget s (o + 2))
    and i3 = ord (uget s (o + 3))
    and i4 = ord (uget s (o + 4))
    and i5 = ord (uget s (o + 5))
    and i6 = ord (uget s (o + 6))
    and i7 = ord (uget s (o + 7)) in

    (i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24) + (i4 lsl 32) + (i5 lsl 40)
     + (i6 lsl 48) + (i7 lsl 56), o + 8)

let const: (('a -> 'b) -> 'a writer) -> 'b -> 'a writer =
  fun w v -> w (fun _ -> v)

let write_literal: string -> 'a writer =
  fun s -> fun b _ -> Buffer.add_string b s
and read_literal: string -> unit reader =
  fun s ->
    let l = String.length s in
    fun i o ->
      check l i o;

      let s' = String.sub i o l in
      if s' <> s
      then failwith "Binary.read_const: unexpected value"
      else ();

      ((), o + l)

let size_string s = size_uint32 + String.length s
and write_string: ('a -> string) -> 'a writer =
  fun f -> fun b a ->
    let s = f a in
    write_uint32_to_buffer b (String.length s);
    Buffer.add_string b s
and read_string: string reader =
  fun s o ->
    let l, o' = read_uint32 s o in
    let s' = String.sub s o' l in
    (s', o' + l)

let size_crc32 = 4
and write_crc32: int -> int option -> 'a writer =
  fun s l -> fun b _ ->
    let l' = match l with
      | None -> Buffer.length b - s
      | Some v -> v
    in

    let d = Buffer.sub b s l' in
    let crc32 = Int32.to_int (Crc32c.calculate_crc32c d 0 l') in
    let crc32' = crc32 + 0x80000000 in

    write_uint32_to_buffer b crc32'
and read_crc32: int reader =
  fun s o ->
    let (v, o') = read_uint32 s o in
    let crc32 = v - 0x80000000 in
    (crc32, o')
and calc_crc32: int -> int option -> int reader =
  fun s l -> fun i o ->
    let l' = match l with
      | None -> o - s
      | Some v -> v
    in

    assert (l' >= 0);

    let crc32 = Int32.to_int (Crc32c.calculate_crc32c i s l') in

    (crc32, o)

let write_list8: ('a writer) -> ('b -> 'a list) -> ('b writer) =
  fun w -> fun f -> fun b a ->
    let os = f a
    and b' = Buffer.create 255
    and c = ref 0 in

    List.iter
      (fun e -> w b' e; incr c) os;

    assert ((!c) <= 0xFF);

    const write_uint8 (!c) b ();
    Buffer.add_buffer b b'
and read_list8: ('a reader) -> ('a list reader) =
  fun r -> fun s o ->
    let c = Char.code (String.get s o) in
    let rec loop acc o' = function
      | 0 -> (List.rev acc, o')
      | n ->
          let (v, o'') = r s o' in
          loop (v :: acc) o'' (pred n)
    in
    loop [] (succ o) c
