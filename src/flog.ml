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

type t = {
  fd_in: file_descr;
  fd_append: file_descr;
  fd_random: file_descr;
  mutable offset: offset;
  mutable root_offset: offset;
  mutable commit_offset: offset;
  mutable closed: bool;

  mutable last_metadata: int;
  mutable metadata: metadata * metadata;
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

let value_tag = chr1
and leaf_tag = chr2
and index_tag = chr3
and commit_tag = chr4

let write_uint8 i s o = String.set s o (Char.chr i)
and read_uint8 s o = Char.code (String.get s o)

let write_uint32 i s o =
  assert (i >= 0 && i < 0x100000000);
  assert (o >= 0 && o <= String.length s - 4);

  let uchr = Char.unsafe_chr
  and uset = String.unsafe_set in

  uset s o (uchr (i land 0xFF));
  uset s (o + 1) (uchr ((i lsr 8) land 0xFF));
  uset s (o + 2) (uchr ((i lsr 16) land 0xFF));
  uset s (o + 3) (uchr ((i lsr 24) land 0xFF))

and read_uint32 s o =
  assert (o >= 0 && String.length s - o >= 4);

  let ord = Char.code
  and uget = String.unsafe_get in

  let i0 = ord (uget s (o + 0))
  and i1 = ord (uget s (o + 1))
  and i2 = ord (uget s (o + 2))
  and i3 = ord (uget s (o + 3)) in

  i0 + (i1 lsl 8) + (i2 lsl 16) + (i3 lsl 24)

let write_crc32 c s o =
  let c' = c + 0x80000000 in
  write_uint32 c' s o
and read_crc32 s o =
  let c = read_uint32 s o in
  c - 0x80000000

let write_uint64 i s o =
  assert (i >= 0 && i <= 0X3FFFFFFFFFFFFFFF); (* max_int *)
  assert (o >= 0 && o <= String.length s - 8);

  let uchr = Char.unsafe_chr
  and uset = String.unsafe_set in

  uset s (o + 0) (uchr ((i lsr  0) land 0xFF));
  uset s (o + 1) (uchr ((i lsr  8) land 0xFF));
  uset s (o + 2) (uchr ((i lsr 16) land 0xFF));
  uset s (o + 3) (uchr ((i lsr 24) land 0xFF));
  uset s (o + 4) (uchr ((i lsr 32) land 0xFF));
  uset s (o + 5) (uchr ((i lsr 40) land 0xFF));
  uset s (o + 6) (uchr ((i lsr 48) land 0xFF));
  uset s (o + 7) (uchr ((i lsr 56) land 0xFF))

and read_uint64 s o =
  assert (0 >= 0 && String.length s - o >= 8);

  let ord = Char.code
  and uget = String.unsafe_get in

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

let lseek_set f o =
  let o' = lseek f o SEEK_SET in
  if o' <> o then failwith "Flog.seek_set: seek failed" else ()

let crc32 s o l = Int32.to_int (Crc32c.calculate_crc32c s o l)

let safe_write f s o l =
  let rec helper o = function
    | 0 -> ()
    | l ->
        let c = Unix.single_write f s o l in
        helper (o + c) (l - c)
  in
  helper o l

let metadata_prefix = "BaArDsKeErDeR"
let metadata_suffix = "bAaRdSkEeRdEr"
let metadata_version = 0x01

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
  write_crc32 crc s (pl + 18 + sl);

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

  let crc = read_crc32 s (pl + 18 + sl) in

  if (crc32 s 0 (pl + 18 + sl) = crc)
  then Some { md_blocksize=bs; md_spindle=sp; md_offset=o; md_count=c }
  else None

let create (f: string) =
  let fd = openfile f [O_WRONLY; O_EXCL; O_CREAT] 0o644 in

  set_close_on_exec fd;
  lockf fd F_TLOCK 0;

  let b = Posix.fstat_blksize fd in

  let metadata1, b1 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=0 }
  and metadata2, b2 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=1 } in

  lseek_set fd 0;
  ftruncate fd (2 * b);
  safe_write fd metadata1 0 b1;
  flush (out_channel_of_descr fd);
  Posix.fsync fd;
  safe_write fd metadata2 0 b2;
  flush (out_channel_of_descr fd);
  Posix.fdatasync fd;

  (* If a database is created, we'll open it soon, most likely *)
  posix_fadvise fd 0 (2 * b) POSIX_FADV_WILLNEED;

  close fd

let serialize_commit o =
  let s = String.create (4 + 1 + 8 + 4) in

  write_uint32 (1 + 8 + 4) s 0;
  String.set s 4 commit_tag;
  write_uint64 o s (4 + 1);
  let crc = crc32 s 0 (4 + 1 + 8) in
  write_crc32 crc s (4 + 1 + 8);

  s

and deserialize_commit s o =
  Commit (read_uint64 s o)

and commit2s (Commit o) = Printf.sprintf "Commit %d" o

let marker = 0x0baadeed
let marker' =
  let s = String.create 4 in
  write_uint32 marker s 0;
  s

let find_commit f o =
  let s = String.create 5 in

  let rec loop a o =
    try
      pread_into_exactly f s 4 o;
      assert (read_uint32 s 0 = marker);

      pread_into_exactly f s 5 (o + 4);

      let s' = read_uint32 s 0 in

      match String.get s 4 with
        | i when i = leaf_tag -> loop a (o + 8 + s')
        | i when i = index_tag -> loop a (o + 8 + s')
        | i when i = value_tag -> loop a (o + 8 + s')
        | i when i = commit_tag -> loop o (o + 8 + s')
        | c -> failwith
                (Printf.sprintf "Flog.find_root: unknown entry type: %d"
                  (Char.code c))

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
  posix_fadvise fd_in 0 size POSIX_FADV_WILLNEED;

  let mds = String.create size in
  pread_into_exactly fd_in mds size 0;
  let md1 = from_some (deserialize_metadata mds) in
  let bs = md1.md_blocksize in
  let mds = String.create bs in
  pread_into_exactly fd_in mds bs bs;
  let md2 = from_some (deserialize_metadata mds) in

  posix_fadvise fd_in 0 (max (2 * 4096) (2 * bs)) POSIX_FADV_DONTNEED;

  (* From now on, the fd_in FD will perform random IO *)
  posix_fadvise fd_in 0 0 POSIX_FADV_RANDOM;

  (* TODO Use 'best' metadata instead of checking it's value *)
  assert (md1.md_blocksize == md2.md_blocksize);
  assert (md1.md_spindle == md2.md_spindle);

  let fd_append = openfile f [O_APPEND; O_WRONLY] 0o644 in
  set_close_on_exec fd_append;

  let offset = lseek fd_append 0 SEEK_END in

  let s =
    if md1.md_count > md2.md_count
    then md1.md_offset
    else md2.md_offset
  in
  let s = if s = 0 then (2 * bs) else s in
  let co = find_commit fd_in s in

  let root =
    if co = 0 then 0
    else begin
      let s = String.create 9 in
      pread_into_exactly fd_in s 9 co;
      assert (read_uint32 s 0 = marker);
      let l = (read_uint32 s 4) - 1 in
      assert (String.get s 8 = commit_tag);
      let s = String.create l in
      pread_into_exactly fd_in s l (co + 9);
      let (Commit root) = deserialize_commit s 0 in
      root end
  in

  (* TODO Choose correct last_metadata *)
  (* TODO Write 'best' metadata into both blocks *)

  { fd_in=fd_in; fd_append=fd_append; fd_random=fd_random; offset=offset;
    commit_offset=co; root_offset=root; closed=false;
    last_metadata=0; metadata=(md1, md2); }

let close db =
  if db.closed
  then ()
  else begin
    Unix.close db.fd_in;
    Unix.close db.fd_append;
    Unix.close db.fd_random;
    db.closed <- true
  end

let int8_placeholder = '0'
let int32_placeholder = "0123"
let int64_placeholder = "01234567"

let serialize_leaf l =
  let b = Buffer.create 256 in

  let s32 = String.create 4
  and s64 = String.create 8 in

  Buffer.add_string b int32_placeholder;
  Buffer.add_char b leaf_tag;
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

  let crc = crc32 s 0 (sl - 4) in
  write_crc32 crc s (sl - 4);

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
  Buffer.add_char b index_tag;
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

  let crc = crc32 s 0 (sl - 4) in
  write_crc32 crc s (sl - 4);

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
  String.set s 4 value_tag;
  String.set s 5 chr0;
  String.blit v 0 s 6 l;

  let crc = crc32 s 0 (tl - 4) in
  write_crc32 crc s (tl - 4);

  s

let deserialize_value s o =
  let options = Char.code (String.get s o) in
  assert (options = 0);

  let sl = String.length s in
  Value (String.sub s (o + 1) (sl - o - 5))

let write t slab =
  let update_offset i = t.offset <- t.offset + i in
  let do_write s =
    safe_write t.fd_append marker' 0 4;
    let sl = String.length s in
    safe_write t.fd_append s 0 sl;
    t.root_offset <- t.offset;
    update_offset (sl + 4)
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
  let s = serialize_commit ro in
  let sl = String.length s in
  safe_write t.fd_append marker' 0 4;
  safe_write t.fd_append s 0 sl;
  update_offset (sl + 4);
  t.commit_offset <- t.offset - sl - 4

let root t = t.root_offset
let next t = t.offset
let read t pos =
  if pos = 0 then NIL
  else
  let s = String.create 9 in

  pread_into_exactly t.fd_in s 9 pos;

  let m = read_uint32 s 0
  and l = read_uint32 s 4 in
  assert (m = marker);

  let s' = String.create (l - 1) in
  pread_into_exactly t.fd_in s' (l - 1) (pos + 4 + 4 + 1);

  match String.get s 8 with
    | i when i = value_tag -> deserialize_value s' 0
    | i when i = leaf_tag -> deserialize_leaf s' 0
    | i when i = index_tag -> deserialize_index s' 0
    | i when i = commit_tag -> deserialize_commit s' 0
    | _ -> failwith "Flog.read: unknown node type"

let sync t =
  (* Retrieve current commit offset *)
  let c = t.commit_offset in

  (* Sync file *)
  flush (out_channel_of_descr t.fd_append);
  Posix.fsync t.fd_append;

  (* Figure out which metadata to overwrite *)
  let i = (t.last_metadata + 1) mod 2 in
  let m = (if i = 0 then fst else snd) t.metadata in

  (* Update metadata *)
  let m' = { m with md_offset=c; md_count=(m.md_count + 2) mod 0xFFFFFFFF } in

  (* Write to disk *)
  let s, bs = serialize_metadata m' in
  lseek_set t.fd_random (if i = 0 then 0 else m.md_blocksize);
  safe_write t.fd_random s 0 bs;

  (* Sync again *)
  flush (out_channel_of_descr t.fd_random);
  Posix.fdatasync t.fd_random;

  (* Update in-memory representation *)
  let md' =
    let a = (if i = 0 then m' else fst t.metadata)
    and b = (if i = 1 then m' else snd t.metadata) in
    (a, b)
  in

  t.metadata <- md';
  t.last_metadata <- i

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
  s.pos <- c + size e + 4;
  c

let clear (t:t) = ()
let string_of_slab s =
  Pretty.string_of_list entry2s s.entries

(* Hole punching compaction *)
module OffsetOrder = struct
  type t = offset
  let compare = Pervasives.compare
end

open OffsetOrder

module OffsetSet = Set.Make(OffsetOrder)

type compact_state = {
  cs_offset: offset;
  cs_entries: OffsetSet.t;
}

let rec compact' =
  let punch =
    let ks = Posix.fallocate_FALLOC_FL_KEEP_SIZE ()
    and ph = Posix.fallocate_FALLOC_FL_PUNCH_HOLE () in

    let fl = ks lor ph in

    fun fd o l ->
      Printf.fprintf Pervasives.stderr "Punch! %d %d\n%!" o l;
      if l = 0 then () else (Posix.fallocate fd fl o l)

  in

  (* do_punch implementation, optimized for blocksize 4096 *)
  let do_punch_4096 f s = function
    | 0 -> ()
    | n ->
        let align_up n = if n mod 4096 = 0 then n else ((n / 4096) + 1) * 4096
        and align_down n = (n / 4096) * 4096 in

        let o1 = align_up s
        and o2 = align_down (s + n) in

        let n' = o2 - o1 in

        if n' <= 0 then () else punch f o1 n'

  and do_punch_generic b f s = function
    | 0 -> ()
    | n ->
        let align_up n = if n mod b = 0 then n else ((n / b) + 1) * b
        and align_down n = (n / b) * b in

        let o1 = align_up s
        and o2 = align_down (s + n) in

        let n' = o2 - o1 in

        if n' <= 0 then () else punch f o1 n'
  in

  fun l b s ->

  let os = s.cs_entries
  and n = s.cs_offset in

  let h' = OffsetSet.max_elt os in
  let os' = OffsetSet.remove h' os in

  let e = read l h' in

  let do_punch =
    let b' = b / 2 in
    if b' = 4096 then do_punch_4096 else do_punch_generic b'
  in

  let (e', os'') = match e with
    | Value _ as v -> (h' + size v + 4, os')
    | Leaf rs as l ->
        (h' + size l + 4,
         List.fold_right OffsetSet.add (List.map snd rs) os')
    | Index (p, kps) as i ->
        (h' + size i + 4,
         List.fold_right OffsetSet.add (List.map snd kps) (OffsetSet.add p os'))
    | Commit _ -> failwith "Flog.compact': Commit entry"
    | NIL -> failwith "Flog.compact': NIL entry"
  in

  do_punch l.fd_random e' (n - e');

  if OffsetSet.is_empty os''
  then do_punch l.fd_random b (h' - b)
  else compact' l b { cs_offset=h'; cs_entries=os''; }


let compact t =
  sync t;

  let md = (if t.last_metadata = 0 then fst else snd) t.metadata in
  let b = md.md_blocksize in

  let b' = b * 2
  and o = t.commit_offset in

  match (read t o) with
    | Commit r ->
        compact' t b' { cs_offset=o; cs_entries=OffsetSet.singleton r; }
    | _ ->
        invalid_arg "Flog.compact: no commit entry at given offset"
