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
open Posix

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

let chr0 = Char.chr 0
let chr1 = Char.chr 1
let chr2 = Char.chr 2
let chr3 = Char.chr 3
let chr4 = Char.chr 4

let write_uint8 i s o =
  assert (i >= 0);
  assert (i < 0x100);
  assert (o <= String.length s - 1);

  String.set s o (Char.chr i)

and read_uint8 s o =
  Char.code (String.get s o)

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

let lseek_set f o =
  let o' = lseek f o SEEK_SET in
  if o' <> o then failwith "Flog.seek_set: seek failed" else ()

let crc32 s o l = 0xDEADBEEF (* TODO *)

let metadata_prefix = "BaArDsKeErDeR"
let metadata_suffix = "bAaRdSkEeRdEr"
let metadata_version = 0x01

type blocksize = int
type spindle = int
type offset = int
type count = int

type metadata = {
  md_blocksize: blocksize;
  md_spindle: spindle;
  md_offset: offset;
  md_count: count;
}

let serialize_metadata md =
  let s = String.make md.md_blocksize chr0
  and pl = String.length metadata_prefix
  and sl = String.length metadata_suffix in
  String.blit metadata_prefix 0 s 0 pl;
  write_uint8 metadata_version s pl;
  write_uint32 md.md_blocksize s (pl + 1);
  write_uint8 md.md_spindle s (pl + 5);
  write_uint64 md.md_offset s (pl + 6);
  write_uint32 md.md_count s (pl + 14);
  String.blit metadata_suffix 0 s (pl + 18) sl;

  let crc = crc32 s 0 (pl + 18 + sl) in
  write_uint32 crc s (pl + 18 + sl);

  (s, md.md_blocksize)

and deserialize_metadata s =
  let p1 = String.sub s 0 (String.length metadata_prefix) in
  assert (p1 = metadata_prefix);

  let pl = String.length p1
  and sl = String.length metadata_suffix in

  let v = read_uint8 s pl in
  assert (v = metadata_version);

  let bs = read_uint32 s (pl + 1) in

  assert (String.length s >= bs);

  let sp = read_uint8 s (pl + 5)
  and o = read_uint64 s (pl + 6)
  and c = read_uint32 s (pl + 14)
  and p2 = String.sub s (pl + 18) sl in

  assert (p2 = metadata_suffix);

  let crc = read_uint32 s (pl + 18 + sl) in

  if (crc32 s 0 (pl + 18 + sl) = crc)
  then Some { md_blocksize=bs; md_spindle=sp; md_offset=o; md_count=c }
  else None

let create (f: string) (b: blocksize) =
  let fd = openfile f [O_WRONLY; O_EXCL; O_CREAT] 0o644
  and metadata1, b1 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=0 }
  and metadata2, b2 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=1 } in

  set_close_on_exec fd;

  lockf fd F_TLOCK 0;

  lseek_set fd 0;
  ftruncate fd (2 * b);
  let _ = write fd metadata1 0 b1 in
  let _ = write fd metadata2 0 b2 in

  close fd

type commit = Commit of offset

let serialize_commit (Commit o) =
  let s = String.create (4 + 1 + 8 + 4) in

  write_uint32 (1 + 8 + 4) s 0;
  String.set s 8 chr4;
  write_uint64 o s (4 + 1);
  let crc = crc32 s (4 + 1 + 8) in
  write_uint32 o s (4 + 1 + 8);

  s

and deserialize_commit s o =
  Commit (read_uint64 s o)

let find_root f o =
  let s = String.create 4
  and t = String.create 1 in

  let rec loop a o =
    try
      pread_into_exactly f s 4 o;
      pread_into_exactly f t 1 (o + 4);

      let s' = read_uint32 s 0 in

      match String.get t 0 with
        | i when i = chr1 -> loop a (o + 4 + s')
        | i when i = chr2 -> loop a (o + 4 + s')
        | i when i = chr3 -> loop a (o + 4 + s')
        | i when i = chr4 ->
            let b = String.create (s' - 1) in
            pread_into_exactly f b (o + 4 + 1) (o + 4 + 1);
            let (Commit c) = deserialize_commit b 0 in
            loop c (o + 4 + s')
        | _ -> failwith "Flog.find_root: unknown entry type"

    with End_of_file ->
      a
  in

  loop 0 o

let make (f: string): t =
  let from_some = function
    | Some x -> x
    | _ -> invalid_arg "Flog.make.from_some"
  in

  let fd_random = openfile f [O_WRONLY;] 0o644 in
  set_close_on_exec fd_random;

  lseek_set fd_random 0;
  lockf fd_random F_TLOCK 0;

  (* TODO Make safe -> close handles! *)
  (* TODO Read metadata *)
  let fd_in = openfile f [O_RDONLY] 0o644 in
  set_close_on_exec fd_in;

  let size = 2 * 4096 in
  let mds = String.create size in
  pread_into_exactly fd_in mds size 0;
  let md1 = from_some (deserialize_metadata mds) in
  let bs = md1.md_blocksize in
  let mds = String.create bs in
  pread_into_exactly fd_in mds bs bs;
  let md2 = from_some (deserialize_metadata mds) in

  (* TODO Use 'best' metadata instead of checking it's value *)
  assert (md1.md_blocksize == md2.md_blocksize);
  assert (md1.md_spindle == md2.md_spindle);

  let fd_append = openfile f [O_APPEND; O_WRONLY] 0o644 in
  set_close_on_exec fd_append;

  let offset = lseek fd_append 0 SEEK_END in

  let root = find_root fd_in (2 * bs) in

  { fd_in=fd_in; fd_append=fd_append; fd_random=fd_random; offset=offset;
    root_offset=root }

let close db =
  Unix.close db.fd_in;
  Unix.close db.fd_append;
  Unix.close db.fd_random

let int8_placeholder = '0'
let int32_placeholder = "0123"
let int64_placeholder = "01234567"

let marker = 0x0baadeed

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
  List.iter write_slab (List.rev slab.entries);

  let ro = t.root_offset in
  let s = serialize_commit (Commit ro) in
  let sl = String.length s in
  let w = write t.fd_append s 0 sl in
  assert (w = sl);
  update_offset sl

let root t = t.root_offset
let next t = t.offset
let read t pos =
  if pos = 0 then NIL
  else
  let s = String.create 4 in

  pread_into_exactly t.fd_in s 4 pos;

  let l = read_uint32 s 0 in
  let s = String.create l in
  pread_into_exactly t.fd_in s l (pos + 4);

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
  Pretty.string_of_list entry2s s.entries