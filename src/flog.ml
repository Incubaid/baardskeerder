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

type blocksize = int
type count = int

module OffsetEntryCache = Entrycache.Make(
  struct
    type k = Pos.pos
    type v = Entry.entry

    let create _ = (Outer (Spindle 0, Offset 0), NIL)
  end)

let (>>) = Binary.(>>)
let (>>=) = Binary.(>>=)
let ($) a b = a b
let id = fun x -> x
let dot f g = fun x -> f $ g x

let chr0 = '\000'
let chr1 = '\001'
let chr2 = '\002'
let chr3 = '\003'
let chr4 = '\004'


(* Metadata handling *)
type metadata = {
  md_blocksize: blocksize;
  md_spindle: spindle;
  md_offset: offset;
  md_count: count;
  md_d : int;
}

let metadata_prefix = "BaArDsKeErDeR"
let metadata_suffix = "bAaRdSkEeRdEr"
let metadata_version = 0x01

let metadata_writer =
  Binary.write_literal metadata_prefix >>
  Binary.const Binary.write_uint8 metadata_version >>
  Binary.write_uint32 (fun md -> md.md_blocksize) >>
  Binary.write_uint8 (fun md -> let (Spindle s) = md.md_spindle in s) >>
  Binary.write_uint64 (fun md -> let (Offset o) = md.md_offset in o) >>
  Binary.write_uint32 (fun md -> md.md_count) >>
  Binary.write_uint32 (fun md -> md.md_d) >>
  Binary.write_literal metadata_suffix >>
  Binary.write_crc32 0 None
and metadata_reader =
  Binary.read_literal metadata_prefix >>= fun () ->
  Binary.read_uint8 >>= fun metadata_version' ->
  if metadata_version' <> metadata_version
  then failwith "Flog.metadata_reader: invalid version"
  else ();
  Binary.read_uint32 >>= fun md_blocksize ->
  Binary.read_uint8 >>= fun md_spindle ->
  Binary.read_uint64 >>= fun md_offset ->
  Binary.read_uint32 >>= fun md_count ->
  Binary.read_uint32 >>= fun md_d ->
  Binary.read_literal metadata_suffix >>= fun () ->
  Binary.calc_crc32 0 None >>= fun crc32 ->
  Binary.read_crc32 >>= fun crc32' ->
  if crc32' <> crc32
  then Binary.return None
  else Binary.return $ Some { md_blocksize=md_blocksize;
                              md_spindle=Spindle md_spindle;
                              md_offset=Offset md_offset;
                              md_count=md_count;
                              md_d=md_d;
                            }

let serialize_metadata md =
  let b, l = Binary.run_writer ~buffer_size:md.md_blocksize
    metadata_writer md in

  assert (l <= md.md_blocksize);

  let s = String.make md.md_blocksize chr0 in
  String.blit b 0 s 0 l;

  (s, md.md_blocksize)
and deserialize_metadata s = fst $ metadata_reader s 0


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

  mutable now: Time.t;
  cache: OffsetEntryCache.t;
}

let lseek_set f o =
  let o' = lseek f o SEEK_SET in
  if o' <> o then failwith "Flog.seek_set: seek failed" else ()

let safe_write f s o l =
  let rec helper o = function
    | 0 -> ()
    | l ->
        let c = Unix.single_write f s o l in
        helper (o + c) (l - c)
  in
  helper o l


let init ?(d=2) (f: string) (_:Time.t) =
  let fd = openfile f [O_WRONLY; O_EXCL; O_CREAT] 0o644 in

  set_close_on_exec fd;
  lockf fd F_TLOCK 0;

  let b = Posix.fstat_blksize fd in

  let metadata1, b1 = serialize_metadata
      { md_blocksize=b; md_spindle=Spindle 0; md_offset=Offset 0; md_count=0; md_d = d }
  and metadata2, b2 = serialize_metadata
      { md_blocksize=b; md_spindle=Spindle 0; md_offset=Offset 0; md_count=1; md_d = d } 
  in

  lseek_set fd 0;
  ftruncate fd (2 * b);
  pwrite_exactly fd metadata1 b1 0;
  flush (out_channel_of_descr fd);
  Posix.fsync fd;
  pwrite_exactly fd metadata2 b2 b1;
  flush (out_channel_of_descr fd);
  Posix.fdatasync fd;

  (* If a database is `init`-ialised, we'll open it soon, most likely *)
  posix_fadvise fd 0 (b1 + b2) POSIX_FADV_WILLNEED;

  close fd

(* Entry handling helpers *)
let reader_envelope: ('a Binary.reader) -> ('a Binary.reader) =
  fun r -> fun s o ->
    let l, _ = Binary.read_uint32 s o in
    if l <> String.length s - Binary.size_uint32
    then failwith "Flog.reader_envelope: size mismatch"
    else ();

    let crc32, _ = Binary.read_crc32 s (String.length s - Binary.size_crc32) in
    let crc32', _ = Binary.calc_crc32 0 (Some (String.length s - Binary.size_crc32)) s 0 in
    if crc32 <> crc32'
    then failwith "Flog.reader_envelope: CRC32 mismatch"
    else ();

    r s Binary.size_uint32
and calculate_size_envelope: ('a -> int) -> 'a -> int =
  fun c -> fun a ->
    c a + Binary.size_uint32 + Binary.size_crc32

let serialize_helper: 'a. ('a Binary.writer) -> 'a -> string =
  fun w -> fun o ->
    let s, l = Binary.run_writer w o in
    let b = Buffer.create (l + Binary.size_uint32 + Binary.size_crc32) in
    Binary.const Binary.write_uint32 (l + Binary.size_crc32) b ();
    Buffer.add_string b s;
    Binary.write_crc32 0 None b ();
    Buffer.contents b
and deserialize_helper: ('a Binary.reader) -> string -> int -> 'a =
  fun r ->
    let r' = reader_envelope r in
    fun s o ->
      fst $ r' s o

(* Entry handling *)
let value_tag = chr1
and leaf_tag = chr2
and index_tag = chr3
and commit_tag = chr4

let calculate_size_commit =
  calculate_size_envelope (fun (_:int) -> Binary.size_char8 + Binary.size_uint64)
and commit_writer =
  let extract = function
    | Inner _ -> failwith "Flog.commit_writer.extract: Inner"
    | Outer (Spindle s, Offset o) -> (s, o)
  in

  Binary.const Binary.write_char8 commit_tag >>
  Binary.write_uint8 (dot fst extract) >>
  Binary.write_uint64 (dot snd extract)
and commit_reader =
  Binary.read_char8 >>= fun t ->
  assert (t = commit_tag);
  Binary.read_uint8 >>= fun s ->
  Binary.read_uint64 >>= fun o ->
  let p = Outer (Spindle s, Offset o)
  and i = Time.zero
  and a = []
  and prev = outer0 (Offset (-2)) in
  let c = Commit.make_commit p prev i a in
  Binary.return $ Commit c

let serialize_commit = serialize_helper commit_writer
and deserialize_commit = deserialize_helper commit_reader


let calculate_size_value =
  calculate_size_envelope
    (fun s -> Binary.size_uint8 + Binary.size_uint8 + Binary.size_string s)
and value_writer =
  Binary.const Binary.write_char8 value_tag >>
  Binary.const Binary.write_uint8 0 >>
  Binary.write_string id
and value_reader =
  Binary.read_char8 >>= fun t ->
  assert (t = value_tag);
  Binary.read_uint8 >>= fun o ->
  assert (o = 0);
  Binary.read_string >>= fun s ->
  Binary.return $ Value s

let serialize_value = serialize_helper value_writer
and deserialize_value = deserialize_helper value_reader


let pos_remap h = function
  | Outer _ as o -> o
  | Inner p -> Hashtbl.find h p

let leaf_writer h =
  Binary.const Binary.write_char8 leaf_tag >>
  Binary.const Binary.write_uint8 0 >>

  let item_writer =
    Binary.write_string fst >>
    Binary.write_uint64 (fun (_, p) -> from_outer0 (pos_remap h p))
  in

  Binary.write_list8 item_writer id
and leaf_reader =
  let item_reader =
    Binary.read_string >>= fun k ->
    Binary.read_uint64 >>= fun p ->
    Binary.return $ (k, outer0 (Offset p))
  in

  Binary.read_char8 >>= fun t ->
  assert (t = leaf_tag);
  Binary.read_uint8 >>= fun o ->
  assert (o = 0);
  Binary.read_list8 item_reader >>= fun kps ->
  Binary.return $ Leaf kps

let serialize_leaf h = serialize_helper (leaf_writer h)
and deserialize_leaf = deserialize_helper leaf_reader


let index_writer h =
  let item_writer =
    Binary.write_string fst >>
    Binary.write_uint64 (fun (_, p) -> from_outer0 (pos_remap h p))
  in

  Binary.const Binary.write_char8 index_tag >>
  Binary.const Binary.write_uint8 0 >>

  Binary.write_uint64 (fun (p0, _) -> from_outer0 (pos_remap h p0)) >>

  Binary.write_list8 item_writer snd >>
  Binary.write_crc32 0 None
and index_reader =
  let item_reader =
    Binary.read_string >>= fun k ->
    Binary.read_uint64 >>= fun p ->
    Binary.return $ (k, outer0 (Offset p))
  in

  Binary.read_char8 >>= fun t ->
  assert (t = index_tag);
  Binary.read_uint8 >>= fun o ->
  assert (o = 0);
  Binary.read_uint64 >>= fun p0 ->
  Binary.read_list8 item_reader >>= fun kps ->

  Binary.return $ Index (outer0 (Offset p0), kps)

let serialize_index h = serialize_helper (index_writer h)
and deserialize_index = deserialize_helper index_reader


let marker = 0x0baadeed
let marker' = fst $ Binary.run_writer ~buffer_size:4
  (Binary.const Binary.write_uint32 marker) ()

let find_commit f o =
  let s = String.create 5 in

  let rec loop a o next_time=
    try
      pread_into_exactly f s 4 o;
      assert (fst $ Binary.read_uint32 s 0 = marker);

      pread_into_exactly f s 5 (o + 4);

      let s' = fst $ Binary.read_uint32 s 0 in
      let o' = o + 8 + s' in
      match fst $ Binary.read_char8 s 4 with
        | i when i = leaf_tag -> loop a o' next_time
        | i when i = index_tag -> loop a o' next_time
        | i when i = value_tag -> loop a o' next_time
        | i when i = commit_tag -> loop o o' next_time
        | c -> failwith
                (Printf.sprintf "Flog.find_root: unknown entry type: %d"
                  (Char.code c))

    with End_of_file ->
      a, next_time
  in

  loop 0 o (Time.zero)

let extend_file =
  let ks = Posix.fallocate_FALLOC_FL_KEEP_SIZE ()
  and extent = 1024 * 1024 * 512 (* 512 MB *) in

  fun fd (Offset offset) ->
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
  assert (md1.md_blocksize = md2.md_blocksize);
  assert (md1.md_spindle = md2.md_spindle);

  let fd_append = openfile f [O_APPEND; O_WRONLY] 0o644 in
  set_close_on_exec fd_append;

  let offset = lseek fd_append 0 SEEK_END in

  let extent = extend_file fd_append (Offset offset) in

  let (Offset s) =
    if md1.md_count > md2.md_count
    then md1.md_offset
    else md2.md_offset
  in
  let s = if s = 0 then (2 * bs) else s in
  let last,now = find_commit fd_in s in

  (* TODO Choose correct last_metadata *)
  (* TODO Write 'best' metadata into both blocks *)

  let cache = OffsetEntryCache.create 32 in

  { fd_in=fd_in; fd_append=fd_append; fd_random=fd_random;
    offset=Offset offset; commit_offset=Offset last;
    closed=false;
    last_metadata=0; metadata=(md1, md2); space_left=extent;
    d = md1.md_d;
    cache=cache;
    now = now;
  }

let get_d (t:t) = t.d

let now (t:t) = t.now

let dump ?(out=Pervasives.stdout) (_:t) = 
  assert (out=out);
  failwith "todo"


let serialize_entry h e = 
  match e with
    | NIL -> failwith "serialize NIL?"
    | Commit c ->
        let pos = Commit.get_pos c in
        let p = pos_remap h pos in
        serialize_commit p
    | Value v -> serialize_value v
    | Index index -> serialize_index h index
    | Leaf leaf -> serialize_leaf h leaf
  

let write t slab =
  let b = Buffer.create 1024 in
  let sl = Slab.length slab in
  let h = Hashtbl.create sl in
  let start = ref (let Offset o = t.offset in o) in
  let rec do_one i e = 
    begin
      let s = serialize_entry h e in
      let size = String.length s + String.length marker' in
      let () = Buffer.add_string b marker' in
      let () = Buffer.add_string b s in
      let () = Hashtbl.replace h i (outer0 (Offset !start)) in
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
  t.commit_offset <- Offset (from_outer0 cp);
  t.offset <- Offset (let Offset o = t.offset in o + l);
  t.space_left <- t.space_left - l

  (* TODO We might want to List.rev slab.es, if slabs get > cache.s big *)
  (* 
     Slab.iter (fun (p, e) -> OffsetEntryCache.add t.cache p e) slab.es
  *)

let last t = outer0 (t.commit_offset)

let unwrap = function
  | Outer _ as o -> o
  | Inner _ -> failwith "Inner?"


let read t pos =
  if pos = Outer (Spindle 0, Offset 0) then NIL
  else
  match OffsetEntryCache.get t.cache pos with
  | Some e -> e
  | None ->
    begin
      let pos' = from_outer0 pos in
      let s = String.create 9 in
      pread_into_exactly t.fd_in s 9 pos';
      
      let m = fst $ Binary.read_uint32 s 0
      and l = fst $ Binary.read_uint32 s 4 in
      assert (m = marker);
      
      let c = fst $ Binary.read_char8 s 8 in
      
      (* Special-case values to get-around a useless allocation + substring *)
      if c = value_tag
      then
        let s' = String.create (l - 2 - 4 - 4) in
        pread_into_exactly t.fd_in s' (l - 2 - 4 - 4)
          (pos' + 4 + 4 + 1 + 1 + 4);
	Value s'
      else
        let s' = String.create (l + 4) in
        pread_into_exactly t.fd_in s' (l + 4) (pos' + 4);

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
  let o = if i = 0 then 0 else m.md_blocksize in
  pwrite_exactly t.fd_random s bs o;

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
    ftruncate db.fd_append (let Offset o = db.offset in o);
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

  fun l pc mb b s ->

  let os = s.cs_entries
  and n = s.cs_offset in
  let n' = (let Offset o = n in o) in

  let h' = OffsetSet.max_elt os in
  let h'' = (let Offset o = h' in o) in
  let os' = OffsetSet.remove h' os in

  let e = read l (Pos.outer0 h') in

  let do_punch =
    if mb = 0 then do_punch_always
    else
      let b' = b / 2 in
      if b' = 4096 then do_punch_4096 else do_punch_generic b'
  in
  let (e', os'') =
    let unpack_offset = function
      | Inner _ -> failwith "Inner"
      | Outer (Spindle 0, o) -> o
      | Outer (Spindle _, _) -> failwith "Non-0 spindle"
    in
    let osnd = dot unpack_offset snd in
    match e with
      | Value v -> (h'' + value_size v + 4, os')
      | Leaf rs ->
	(h'' + leaf_size rs + 4,
	 List.fold_right OffsetSet.add (List.map osnd rs) os')
      | Index ((p0, kps) as i) ->
	(h'' + index_size i + 4,
	 List.fold_right OffsetSet.add (List.map osnd kps)
             (OffsetSet.add (unpack_offset p0) os'))
      | Commit _ -> failwith "Flog.compact': Commit entry"
      | NIL -> failwith "Flog.compact': NIL entry"
  in

  let cb i =
    match pc with
      | None -> ()
      | Some fn -> fn (l.offset) (Offset i)
  in

  cb e';
  do_punch l.fd_random mb e' (n' - e');

  if OffsetSet.is_empty os''
  then begin
    cb b;
    do_punch l.fd_random mb b (h'' - b)
  end
  else compact' l pc mb b { cs_offset=h'; cs_entries=os''; }


let compact ?min_blocks:(mb=0) ?progress_cb:(pc=None) t =
  sync t;

  let md = (if t.last_metadata = 0 then fst else snd) t.metadata in
  let b = md.md_blocksize in

  let b' = b * 2
  and o = t.commit_offset in

  match (read t (Pos.outer0 o)) with
    | Commit c ->
      let r = Commit.get_pos c in
      let p = from_outer0 (unwrap r) in
      compact' t pc mb b' { cs_offset=o; cs_entries=OffsetSet.singleton $ Offset p; }
    | NIL ->
        failwith "Flog.compact: read NIL iso commit entry"
    | Leaf _ | Value _ | Index _ ->
        invalid_arg "Flog.compact: no commit entry at given offset"
