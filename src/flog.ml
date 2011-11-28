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
open Pos
open Posix

open Flog_serialization

type blocksize = int
type spindle = int
type offset = int
type count = int

type metadata = {
  md_blocksize: blocksize;
  md_spindle: spindle;
  md_offset: offset;
  md_count: count;
  md_d : int;
}

module OffsetEntryCache = Entrycache.Make(
  struct
    type k = offset
    type v = Entry.entry

    let create _ = (0, NIL)
  end)

type t = {
  fd_in: file_descr;
  fd_append: file_descr;
  fd_random: file_descr;
  d : int;
  mutable offset: offset;
  mutable commit_offset: offset;
  mutable closed: bool;

  mutable last_metadata: int;
  mutable metadata: metadata * metadata;

  mutable space_left: int;

  cache: OffsetEntryCache.t;
}

let chr0 = '\000'
let chr1 = '\001'
let chr2 = '\002'
let chr3 = '\003'
let chr4 = '\004'

let value_tag = chr1
and leaf_tag = chr2
and index_tag = chr3
let commit_tag = chr4

let size_crc32 = size_uint32
and write_crc32 c s o =
  let c' = c + 0x80000000 in
  write_uint32 c' s o
and read_crc32 s o =
  let c = read_uint32 s o in
  c - 0x80000000

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
  write_uint32 md.md_d s (pl + 18);
  String.blit metadata_suffix 0 s (pl + 22) sl;

  let crc = crc32 s 0 (pl + 22 + sl) in
  write_crc32 crc s (pl + 22 + sl);

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
  and d = read_uint32 s (pl + 18)
  and p2 = String.sub s (pl + 22) sl in

  assert (p2 = metadata_suffix);

  let crc = read_crc32 s (pl + 22 + sl) in

  if (crc32 s 0 (pl + 22 + sl) = crc)
  then Some { md_blocksize=bs; md_spindle=sp; md_offset=o; md_count=c; md_d = d }
  else None

let init ?(d=2) (f: string) =
  let fd = openfile f [O_WRONLY; O_EXCL; O_CREAT] 0o644 in

  set_close_on_exec fd;
  lockf fd F_TLOCK 0;

  let b = Posix.fstat_blksize fd in

  let metadata1, b1 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=0; md_d = d }
  and metadata2, b2 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset=0; md_count=1; md_d = d } 
  in

  lseek_set fd 0;
  ftruncate fd (2 * b);
  safe_write fd metadata1 0 b1;
  flush (out_channel_of_descr fd);
  Posix.fsync fd;
  safe_write fd metadata2 0 b2;
  flush (out_channel_of_descr fd);
  Posix.fdatasync fd;

  (* If a database is `init`-ialised, we'll open it soon, most likely *)
  posix_fadvise fd 0 (2 * b) POSIX_FADV_WILLNEED;

  close fd

let calculate_size_commit (_:int) = size_uint32 + size_uint8 + size_uint64 + size_crc32

let serialize_commit o = 
  let l = calculate_size_commit o in

  let s = String.create l in
  
  write_uint32 (l - 4) s 0;
  write_char8 commit_tag s size_uint32;
  write_uint64 o s (size_uint32 + size_uint8);

  let crc = crc32 s 0 (size_uint32 + size_uint8 + size_uint64) in
  write_crc32 crc s (size_uint32 + size_uint8 + size_uint64);
  s

and deserialize_commit s o =
  let i = (read_uint64 s o) in
  Commit (Outer i)


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

      match read_char8 s 4 with
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

let extend_file =
  let ks = Posix.fallocate_FALLOC_FL_KEEP_SIZE ()
  and extent = 1024 * 1024 * 512 (* 512 MB *) in

  fun fd offset ->
    Posix.fallocate fd ks offset extent;
    extent

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

  let tbs = Posix.fstat_blksize fd_in in

  posix_fadvise fd_in 0 (2 * tbs) POSIX_FADV_WILLNEED;

  let mds = String.create tbs in
  pread_into_exactly fd_in mds tbs 0;
  let md1 = from_some (deserialize_metadata mds) in
  let bs = md1.md_blocksize in
  let mds = String.create bs in
  pread_into_exactly fd_in mds bs bs;
  let md2 = from_some (deserialize_metadata mds) in

  posix_fadvise fd_in 0 (max tbs (2 * bs)) POSIX_FADV_DONTNEED;

  (* TODO Use 'best' metadata instead of checking it's value *)
  assert (md1.md_blocksize == md2.md_blocksize);
  assert (md1.md_spindle == md2.md_spindle);

  let fd_append = openfile f [O_APPEND; O_WRONLY] 0o644 in
  set_close_on_exec fd_append;

  let offset = lseek fd_append 0 SEEK_END in

  let extent = extend_file fd_append offset in

  let s =
    if md1.md_count > md2.md_count
    then md1.md_offset
    else md2.md_offset
  in
  let s = if s = 0 then (2 * bs) else s in
  let last = find_commit fd_in s in

  (* TODO Choose correct last_metadata *)
  (* TODO Write 'best' metadata into both blocks *)

  let cache = OffsetEntryCache.create 32 in

  { fd_in=fd_in; fd_append=fd_append; fd_random=fd_random; offset=offset;
    commit_offset=last; closed=false;
    last_metadata=0; metadata=(md1, md2); space_left=extent;
    d = md1.md_d;
    cache=cache;
  }

let get_d (t:t) = t.d

let dump ?(out=Pervasives.stdout) (_:t) = 
  assert (out=out);
  failwith "todo"

let int8_placeholder = '0'
let int32_placeholder = "0123"
let int64_placeholder = "01234567"

let pos_remap h = function
  | Outer p -> p
  | Inner p -> Hashtbl.find h p


let serialize_leaf h l =
  let b = Buffer.create 256 in

  let s32 = String.create 4
  and s64 = String.create 8 in

  Buffer.add_string b int32_placeholder;
  Buffer.add_char b leaf_tag;
  Buffer.add_char b chr0;

  Buffer.add_char b int8_placeholder;

  let c = ref 0 in
  List.iter (fun (k, pos) ->
    write_uint32 (String.length k) s32 0;
    Buffer.add_string b s32;
    Buffer.add_string b k;
    let p = pos_remap h pos in
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
  let options = read_uint8 s o in
  assert (options = 0);

  let count = read_uint8 s (o + 1) in

  let rec loop acc o = function
    | 0 -> List.rev acc
    | n ->
        let kl = read_uint32 s o in
        let k = String.sub s (o + 4) kl in
        let p = read_uint64 s (o + 4 + kl) in
	let pos = Outer p in
        loop ((k, pos) :: acc) (o + 4 + kl + 8) (pred n)
  in

  Leaf (loop [] (o + 2) count)

let serialize_index h (p0, kps) =
  let b = Buffer.create 256 in

  let s32 = String.create 4
  and s64 = String.create 8 in

  Buffer.add_string b int32_placeholder;
  Buffer.add_char b index_tag;
  Buffer.add_char b chr0;

  Buffer.add_string b int64_placeholder;

  Buffer.add_char b int8_placeholder;

  let c = ref 0 in
  List.iter(fun (k, (pos:Pos.pos)) ->
    write_uint32 (String.length k) s32 0;
    Buffer.add_string b s32;
    Buffer.add_string b k;
    let p = pos_remap h pos in
    write_uint64 p s64 0;
    Buffer.add_string b s64;
    incr c;
  ) kps;

  Buffer.add_string b int32_placeholder;

  let s = Buffer.contents b in
  let sl = String.length s in

  write_uint32 (sl - 4) s 0;
  let p = pos_remap h p0 in
  write_uint64 p s 6;
  write_uint8 (!c) s 14;

  let crc = crc32 s 0 (sl - 4) in
  write_crc32 crc s (sl - 4);

  s

let deserialize_index s o =
  let options = read_uint8 s o in
  assert (options = 0);

  let p0 = read_uint64 s (o + 1) in
  let pos0 = Outer p0 in
  let count = read_uint8 s (o + 9) in

  let rec loop acc o = function
    | 0 -> List.rev acc
    | n ->
        let kl = read_uint32 s o in
        let k = String.sub s (o + 4) kl in
        let p = read_uint64 s (o + 4 + kl) in
	let pos = Outer p in
        loop ((k, pos) :: acc) (o + 4 + kl + 8) (pred n)
  in

  Index (pos0, loop [] (o + 10) count)


let calculate_size_value v = size_uint32 + size_uint8 + size_uint8 + String.length v + size_crc32


let serialize_value v = 
  let l = calculate_size_value v in
  let s = String.create l in
  let sl = String.length v in
  
  write_uint32 (l - 4) s 0;
  write_char8 value_tag s size_uint32;
  write_uint8 0 s (size_uint32 + size_uint8);
  String.blit v 0 s (size_uint32 + size_uint8 + size_uint8) sl;
  
  let crc = crc32 s 0 (l - 4) in
  write_crc32 crc s (size_uint32 + size_uint8 + size_uint8 + sl);
  s


and deserialize_value s o =
  let options = read_uint8 s o in
  assert (options = 0);

  let sl = String.length s in
  Value (String.sub s (o + 1) (sl - o - 5))



  
let serialize_entry h e = 
  match e with
    | NIL -> failwith "serialize NIL?"
    | Commit pos -> serialize_commit (pos_remap h pos)
    | Value v -> serialize_value v
    | Index index -> serialize_index h index
    | Leaf leaf -> serialize_leaf h leaf
  

let write t slab =
  let b = Buffer.create 1024 in
  let sl = Slab.length slab in
  let h = Hashtbl.create sl in
  let start = ref t.offset in
  let rec do_one i e = 
    begin
      let s = serialize_entry h e in
      let size = String.length s + String.length marker' in
      let () = Buffer.add_string b marker' in
      let () = Buffer.add_string b s in
      let () = Hashtbl.replace h i !start in
      let () = start := !start + size in
      ()
    end
  in
  let () = Slab.iteri slab do_one in
  let cp = Hashtbl.find h (sl -1) in
  (* as before *)
  let s = Buffer.contents b in
  let l = String.length s in
  if t.space_left < l
  then
    let e = extend_file t.fd_append t.offset in
    t.space_left <- e
  else
    ();

  safe_write t.fd_append s 0 l;
  t.commit_offset <- cp;
  t.offset <- t.offset + l;
  t.space_left <- t.space_left - l

  (* TODO We might want to List.rev slab.es, if slabs get > cache.s big *)
  (* 
     Slab.iter (fun (p, e) -> OffsetEntryCache.add t.cache p e) slab.es
  *)

let last t = Outer t.commit_offset

let next t = Outer t.offset

let unwrap = function
  | Outer p -> p
  | Inner _ -> failwith "Inner?"


let read t w =
  let pos = unwrap w in
  if pos = 0 then NIL
  else
  match OffsetEntryCache.get t.cache pos with
  | Some e -> e
  | None ->
    begin
      let s = String.create 9 in
      pread_into_exactly t.fd_in s 9 pos;
      
      let m = read_uint32 s 0
      and l = read_uint32 s 4 in
      assert (m = marker);
      
      let c = read_char8 s 8 in
      
      (* Special-case values to get-around a useless allocation + substring *)
      if c = value_tag
      then
	let s' = String.create (l - 2 - 4) in
	pread_into_exactly t.fd_in s' (l - 2 - 4)
	  (pos + 4 + 4 + 1 + 1);
	Value s'
      else
	let s' = String.create (l - 1) in
	pread_into_exactly t.fd_in s' (l - 1) (pos + 4 + 4 + 1);
	
	match c with
	  | i when i = value_tag -> failwith "Flog.read: value"
	  | i when i = leaf_tag -> deserialize_leaf s' 0
	  | i when i = index_tag -> deserialize_index s' 0
	  | i when i = commit_tag -> deserialize_commit s' 0
	  | _ -> failwith "Flog.read: unknown node type"
    end

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


let close db =
  if db.closed
  then ()
  else begin
    sync db;
    Unix.close db.fd_in;
    ftruncate db.fd_append db.offset;
    Unix.close db.fd_append;
    Unix.close db.fd_random;
    db.closed <- true
  end

let clear _ = ()

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

let value_size v = String.length (serialize_value v)
let leaf_size l  = 
  let h0 = Hashtbl.create 1 in
  String.length (serialize_leaf h0 l) 
let index_size i = 
  let h0 = Hashtbl.create 1 in
  String.length (serialize_index h0 i)


let rec compact' =
  let punch =
    let ks = Posix.fallocate_FALLOC_FL_KEEP_SIZE ()
    and ph = Posix.fallocate_FALLOC_FL_PUNCH_HOLE () in

    let fl = ks lor ph in

    fun fd o l ->
      if l = 0 then () else (Posix.fallocate fd fl o l)

  in

  (* do_punch implementation, optimized for blocksize 4096 *)
  let do_punch_4096 f m s = function
    | 0 -> ()
    | n ->
        let align_up n = if n mod 4096 = 0 then n else ((n / 4096) + 1) * 4096
        and align_down n = (n / 4096) * 4096 in

        let o1 = align_up s
        and o2 = align_down (s + n) in

        let n' = o2 - o1 in

        if n' <= 0 || (n' / 4096 < m) then () else punch f o1 n'

  and do_punch_generic b f m s = function
    | 0 -> ()
    | n ->
        let align_up n = if n mod b = 0 then n else ((n / b) + 1) * b
        and align_down n = (n / b) * b in

        let o1 = align_up s
        and o2 = align_down (s + n) in

        let n' = o2 - o1 in

        if n' <= 0 || (n' / b < m) then () else punch f o1 n'

  and do_punch_always f _ s = function
    | 0 -> ()
    | n -> punch f s n
  in

  fun l mb b s ->

  let os = s.cs_entries
  and n = s.cs_offset in

  let h' = OffsetSet.max_elt os in
  let os' = OffsetSet.remove h' os in

  let e = read l (Pos.out h') in

  let do_punch =
    if mb = 0 then do_punch_always
    else
      let b' = b / 2 in
      if b' = 4096 then do_punch_4096 else do_punch_generic b'
  in
  let (e', os'') = 
    let osnd (_,pos ) = unwrap pos in
    match e with
      | Value v -> (h' + value_size v + 4, os')
      | Leaf rs ->
	(h' + leaf_size rs + 4,
	 List.fold_right OffsetSet.add (List.map osnd rs) os')
      | Index ((p0, kps) as i) ->
	(h' + index_size i + 4,
	 List.fold_right OffsetSet.add (List.map osnd kps) (OffsetSet.add (unwrap p0) os'))
      | Commit _ -> failwith "Flog.compact': Commit entry"
      | NIL -> failwith "Flog.compact': NIL entry"
  in

  do_punch l.fd_random mb e' (n - e');

  if OffsetSet.is_empty os''
  then do_punch l.fd_random mb b (h' - b)
  else compact' l mb b { cs_offset=h'; cs_entries=os''; }


let compact ?min_blocks:(mb=0) t =
  sync t;

  let md = (if t.last_metadata = 0 then fst else snd) t.metadata in
  let b = md.md_blocksize in

  let b' = b * 2
  and o = t.commit_offset in

  match (read t (Pos.out o)) with
    | Commit r ->
      let p = unwrap r in
      compact' t mb b' { cs_offset=o; cs_entries=OffsetSet.singleton p; }
    | NIL ->
        failwith "Flog.compact: read NIL iso commit entry"
    | Leaf _ | Value _ | Index _ ->
        invalid_arg "Flog.compact: no commit entry at given offset"
