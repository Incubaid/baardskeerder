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

open Unix

open Entry

type t = {
  fd_in: file_descr;
  fd_append: file_descr;
  fd_random: file_descr;
  mutable offset: int;
  mutable root_offset: int;
}

type slab = {
  mutable entries: entry list;
  mutable pos: int;
}

let lseek_set f o =
  let o' = lseek f o SEEK_SET in
  if o' <> o then failwith "Flog.seek_set: seek failed" else ()

let create f b =
  let fd = openfile f [O_WRONLY; O_EXCL; O_CREAT] 0o644
  and meta_garbage = String.make b '.' in

  set_close_on_exec fd;

  let _ = write fd "." 0 1 in
  lockf fd F_TLOCK 1;

  lseek_set fd 0;
  ftruncate fd (2 * b);
  let _ = write fd meta_garbage 0 b in
  let _ = write fd meta_garbage 0 b in

  close fd

let make (f: string): t =
  let fd_random = openfile f [O_WRONLY;] 0o644 in
  set_close_on_exec fd_random;

  lseek_set fd_random 0;
  lockf fd_random F_TLOCK 1;

  (* TODO Make safe -> close handles! *)
  (* TODO Read metadata *)
  let fd_in = openfile f [O_RDONLY] 0o644 in
  set_close_on_exec fd_in;
  let fd_append = openfile f [O_APPEND; O_WRONLY] 0o644 in
  set_close_on_exec fd_append;

  let offset = lseek fd_append 0 SEEK_END in

  { fd_in=fd_in; fd_append=fd_append; fd_random=fd_random; offset=offset;
    root_offset=(2 * 4096) }

let chr0 = Char.chr 0
let chr1 = Char.chr 1
let chr2 = Char.chr 2
let chr3 = Char.chr 3

let int8_placeholder = '0'
let int32_placeholder = "0123"
let int64_placeholder = "01234567"

let write_uint8 i s o =
  assert (i >= 0);
  assert (i < 0x100);
  assert (o <= String.length s - 1);

  String.set s o (Char.chr i)

let write_uint32 i s o =
  assert (i >= 0);
  assert (i < 0x100000000);
  assert (o <= String.length s - 4);

  String.set s o (Char.chr (i land 0xFF));
  String.set s (o + 1) (Char.chr ((i lsr 8) land 0xFF));
  String.set s (o + 2) (Char.chr ((i lsr 16) land 0xFF));
  String.set s (o + 3) (Char.chr ((i lsr 24) land 0xFF))

let read_uint32 s o =
  let l = String.length s in
  assert (l - o >= 4);

  let ord = Char.code in

  let i0 = ord (String.get s o)
  and i1 = ord (String.get s (o + 1))
  and i2 = ord (String.get s (o + 2))
  and i3 = ord (String.get s (o + 3)) in

  i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24)

let write_uint64 i s o =
  assert (i >= 0);
  assert (i <= 0X3FFFFFFFFFFFFFFF); (* max_int *)
  assert (o <= String.length s - 8);

  String.set s o (Char.chr (i land 0xFF));
  String.set s (o + 1) (Char.chr ((i lsr 8) land 0xFF));
  String.set s (o + 2) (Char.chr ((i lsr 16) land 0xFF));
  String.set s (o + 3) (Char.chr ((i lsr 24) land 0xFF));
  String.set s (o + 4) (Char.chr ((i lsr 32) land 0xFF));
  String.set s (o + 5) (Char.chr ((i lsr 40) land 0xFF));
  String.set s (o + 6) (Char.chr ((i lsr 48) land 0xFF));
  String.set s (o + 7) (Char.chr ((i lsr 56) land 0xFF))

let read_uint64 s o =
  let l = String.length s in
  assert (l - o >= 8);

  let ord = Char.code in

  let i0 = ord (String.get s o)
  and i1 = ord (String.get s (o + 1))
  and i2 = ord (String.get s (o + 2))
  and i3 = ord (String.get s (o + 3))
  and i4 = ord (String.get s (o + 4))
  and i5 = ord (String.get s (o + 5))
  and i6 = ord (String.get s (o + 6))
  and i7 = ord (String.get s (o + 7)) in

  i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24) + (i4 lsl 32) + (i5 lsl 40)
    + (i6 lsl 48) + (i7 lsl 56)

let serialize_leaf l =
  let b = Buffer.create 256 in

  let s32 = String.create 4
  and s64 = String.create 8 in

  Buffer.add_string b int32_placeholder;
  Buffer.add_char b chr2;
  Buffer.add_char b chr0;

  Buffer.add_char b int8_placeholder;

  let c = ref 0 in
  List.iter (fun (k, p) ->
    write_uint32 (String.length k) s32 0;
    Buffer.add_string b s32;
    Buffer.add_string b k;
    write_uint64 p s64 0;
    Buffer.add_string b s64;
    incr c;
  ) l;

  Buffer.add_string b int32_placeholder;

  let s = Buffer.contents b in
  let sl = String.length s in

  write_uint32 (sl - 4) s 0;
  write_uint8 (!c) s 6;
  write_uint32 0 s (sl - 4); (* TODO *)

  s

let deserialize_leaf s o =
  let options = Char.code (String.get s o) in
  assert (options = 0);

  let count = Char.code (String.get s (o + 1)) in

  let rec loop acc o = function
    | 0 -> List.rev acc
    | n ->
        let kl = read_uint32 s o in
        let k = String.sub s (o + 4) kl in
        let p = read_uint64 s (o + 4 + kl) in
        loop ((k, p) :: acc) (o + 4 + kl + 8) (pred n)
  in

  Leaf (loop [] (o + 2) count)

let serialize_index (p, kps) =
  let b = Buffer.create 256 in

  let s32 = String.create 4
  and s64 = String.create 8 in

  Buffer.add_string b int32_placeholder;
  Buffer.add_char b chr3;
  Buffer.add_char b chr0;

  Buffer.add_string b int64_placeholder;

  Buffer.add_char b int8_placeholder;

  let c = ref 0 in
  List.iter(fun (k, p) ->
    write_uint32 (String.length k) s32 0;
    Buffer.add_string b s32;
    Buffer.add_string b k;
    write_uint64 p s64 0;
    Buffer.add_string b s64;
    incr c;
  ) kps;

  Buffer.add_string b int32_placeholder;

  let s = Buffer.contents b in
  let sl = String.length s in

  write_uint32 (sl - 4) s 0;
  write_uint64 p s 6;
  write_uint8 (!c) s 14;
  write_uint32 0 s (sl - 4); (* TODO *)

  s

let deserialize_index s o =
  let options = Char.code (String.get s o) in
  assert (options = 0);

  let p = read_uint64 s (o + 1) in

  let count = Char.code (String.get s (o + 9)) in

  let rec loop acc o = function
    | 0 -> List.rev acc
    | n ->
        let kl = read_uint32 s o in
        let k = String.sub s (o + 4) kl in
        let p = read_uint64 s (o + 4 + kl) in
        loop ((k, p) :: acc) (o + 4 + kl + 8) (pred n)
  in

  Index (p, loop [] (o + 10) count)

let serialize_value v =
  let l = String.length v in
  let tl = 10 + l in
  let s = String.create tl
  and el = tl - 4 in

  write_uint32 el s 0;
  String.set s 4 chr1;
  String.set s 5 chr0;
  String.blit v 0 s 6 l;
  (* TODO CRC32 *)
  write_uint32 0 s (tl - 4);

  s

let deserialize_value s o =
  let options = Char.code (String.get s o) in
  assert (options = 0);

  let sl = String.length s in
  Value (String.sub s (o + 1) (sl - o - 5))

let write t slab =
  let update_offset i = t.offset <- t.offset + i in
  let do_write s =
    let sl = String.length s in
    let w = write t.fd_append s 0 sl in
    assert (w = sl);
    t.root_offset <- t.offset;
    update_offset sl
  in

  let write_slab = function
    | NIL -> failwith "Flog.write: NIL entry"
    | Value v ->
        let s = serialize_value v in
        do_write s
        (* TODO Assert offset is actually what was calculated *)
    | Leaf l ->
        let s = serialize_leaf l in
        do_write s
    | Index i ->
        let s = serialize_index i in
        do_write s
  in
  List.iter write_slab (List.rev slab.entries)

let root t = t.root_offset
let next t = t.offset
let read t pos =
  let s = String.create 4 in

  (* TODO Use pread(2) *)
  lseek_set t.fd_in pos;

  let c = read t.fd_in s 0 4 in
  assert (c = 4);

  let l = read_uint32 s 0 in
  let s = String.create l in
  let c = read t.fd_in s 0 l in
  assert (c = l);

  Printf.printf "s = %s\n" s;
  match String.get s 0 with
    | i when i = chr1 -> deserialize_value s 1
    | i when i = chr2 -> deserialize_leaf s 1
    | i when i = chr3 -> deserialize_index s 1
    | _ -> failwith "Flog.read: unknown node type"

(* TODO Don't really need to serialize! *)
let size = function
  | NIL -> failwith "Flog.size: NIL entry"
  | Value v -> String.length (serialize_value v)
  | Leaf l -> String.length (serialize_leaf l)
  | Index i -> String.length (serialize_index i)

let make_slab t = { entries=[]; pos=next t }
let add s e =
  s.entries <- e :: s.entries;
  let c = s.pos in
  s.pos <- c + size e;
  c

let string_of_slab s =
  Pretty.string_of_list string_of_entry s.entries
