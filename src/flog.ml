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

open Entry
open Pos

type blocksize = int
type count = int

module OffsetEntryCache = Entrycache.Make(
  struct
    type k = Pos.pos
    type v = Entry.entry

    let create _ = (Outer (0, 0), NIL)
  end)


(* Metadata handling *)
type metadata = {
  md_blocksize: blocksize;
  md_spindle: spindle;
  md_offset: offset;
  md_count: count;
  md_d : int;
}

let pos_remap h = function
  | Outer _ as o -> o
  | Inner p -> Hashtbl.find h p


let chr0 = '\000'
let chr1 = '\001'
let chr2 = '\002'
let chr3 = '\003'
let chr4 = '\004'

let ($) a b = a b
let id = fun x -> x
let dot f g = fun x -> f $ g x

module SerDes = struct

  let (>>) = Binary.(>>)
  let (>>=) = Binary.(>>=)

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


  let metadata_prefix = "BaArDsKeErDeR"
  let metadata_suffix = "bAaRdSkEeRdEr"
  let metadata_version = 0x01

  let metadata_writer =
    Binary.write_literal metadata_prefix >>
      Binary.const Binary.write_uint8 metadata_version >>
      Binary.write_uint32 (fun md -> md.md_blocksize) >>
      Binary.write_uint8  (fun md -> md.md_spindle) >>
      Binary.write_uint64 (fun md -> md.md_offset) >>
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
    else Binary.return $ Some { md_blocksize;
                                md_spindle;
                                md_offset;
                                md_count;
                                md_d;
                              }


  let write_pos =
    fun f -> fun b a ->
      match f a with
        | Inner _ -> failwith "Flog.write_pos: Inner"
        | Outer (s, o) ->
            Binary.write_uint8 (fun () -> s) b ();
            Binary.write_uint64 (fun () -> o) b ()
  and read_pos =
    Binary.read_uint8 >>= fun s ->
    Binary.read_uint64 >>= fun o ->
    Binary.return $ Outer (s,o)

  (* Entry handling *)
  let value_tag = chr1
  and leaf_tag = chr2
  and index_tag = chr3
  and commit_tag = chr4

  let _calculate_size_commit =
    calculate_size_envelope (fun (_:int) -> Binary.size_char8 + Binary.size_uint64)
  and commit_writer =
    Binary.const Binary.write_char8 commit_tag >>
      write_pos id
  and commit_reader =
    Binary.read_char8 >>= fun t ->
    assert (t = commit_tag);
    read_pos >>= fun pos ->
    let i = Time.zero
    and a = []
    and previous = Outer (0, (-2)) in
    let lookup = pos in
    let explicit = false in
    let c = Commit.make_commit ~pos ~previous ~lookup i a explicit in
    Binary.return $ Commit c


  let _calculate_size_value =
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


  let leaf_writer h =
    Binary.const Binary.write_char8 leaf_tag >>
      Binary.const Binary.write_uint8 0 >>

      let item_writer =
        Binary.write_string fst >>
          write_pos (fun (_, p) -> pos_remap h p)
      in

      Binary.write_list8 item_writer id
  and leaf_reader =
    let item_reader =
      Binary.read_string >>= fun k ->
      read_pos >>= fun p ->
      Binary.return (k, p)
    in

    Binary.read_char8 >>= fun t ->
    assert (t = leaf_tag);
    Binary.read_uint8 >>= fun o ->
    assert (o = 0);
    Binary.read_list8 item_reader >>= fun kps ->
    Binary.return $ Leaf kps


  let index_writer h =
    let item_writer =
      Binary.write_string fst >>
        write_pos (dot (pos_remap h) snd)
    in

    Binary.const Binary.write_char8 index_tag >>
      Binary.const Binary.write_uint8 0 >>

      write_pos (dot (pos_remap h) fst) >>

      Binary.write_list8 item_writer snd >>
      Binary.write_crc32 0 None
  and index_reader =
    let item_reader =
      Binary.read_string >>= fun k ->
      read_pos >>= fun p ->
      Binary.return (k, p)
    in

    Binary.read_char8 >>= fun t ->
    assert (t = index_tag);
    Binary.read_uint8 >>= fun o ->
    assert (o = 0);
    read_pos >>= fun p0 ->
    Binary.read_list8 item_reader >>= fun kps ->

    Binary.return $ Index (p0, kps)

end


let serialize_metadata md =
  let b, l = Binary.run_writer ~buffer_size:md.md_blocksize
    SerDes.metadata_writer md in

  assert (l <= md.md_blocksize);

  let s = String.make md.md_blocksize chr0 in
  String.blit b 0 s 0 l;

  (s, md.md_blocksize)
and deserialize_metadata s = fst $ SerDes.metadata_reader s 0


module Flog =
  functor(S:Bs_internal.STORE) ->
struct

  type 'a m = 'a S.m
  let bind = S.bind
  and (>>=) = S.bind
  and return = S.return

  type t = {
    fd: S.t;
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
    let o' = Unix.lseek f o Unix.SEEK_SET in
    if o' <> o then failwith "Flog.seek_set: seek failed" else ()

    (* TODO Use time argument! *)
  let init ?(d=2) (f: string) (_:Time.t) =
    S.init f >>= fun fd ->

    S.with_fd fd Unix.set_close_on_exec >>= fun () ->
    S.with_fd fd (fun fd -> Unix.lockf fd Unix.F_TLOCK 0) >>= fun () ->

    S.with_fd fd Posix.fstat_blksize >>= fun b ->

    let metadata1, b1 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset= 0; md_count=0; md_d = d }
    and metadata2, b2 = serialize_metadata
      { md_blocksize=b; md_spindle=0; md_offset= 0; md_count=1; md_d = d }
    in

    S.with_fd fd
      (fun fd ->
        lseek_set fd 0;
        Unix.ftruncate fd (2 * b)
      ) >>= fun () ->

    S.write fd metadata1 0 b1 0 >>= fun () ->
    S.fsync fd >>= fun () ->
    S.write fd metadata2 0 b2 b1 >>= fun () ->
    S.with_fd fd Posix.fdatasync >>= fun () ->

      (* If a database is `init`-ialised, we'll open it soon, most likely *)
    S.with_fd fd (fun fd -> Posix.posix_fadvise fd 0 (b1 + b2) Posix.POSIX_FADV_WILLNEED)
    >>= fun () ->

    S.close fd



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
      let r' = SerDes.reader_envelope r in
      fun s o ->
        fst $ r' s o


  let serialize_commit = serialize_helper SerDes.commit_writer
  and deserialize_commit = deserialize_helper SerDes.commit_reader

  let serialize_value = serialize_helper SerDes.value_writer
  and _deserialize_value = deserialize_helper SerDes.value_reader

  let serialize_leaf h = serialize_helper (SerDes.leaf_writer h)
  and deserialize_leaf = deserialize_helper SerDes.leaf_reader

  let serialize_index h = serialize_helper (SerDes.index_writer h)
  and deserialize_index = deserialize_helper SerDes.index_reader


  let marker = 0x0baadeed
  let marker' = fst $ Binary.run_writer ~buffer_size:4
    (Binary.const Binary.write_uint32 marker) ()

  let find_commit f o =
    let rec loop a o next_time=
      try
        S.read f o 4 >>= fun s ->
        assert (fst $ Binary.read_uint32 s 0 = marker);

        S.read f (o + 4) 5 >>= fun s ->

        let s' = fst $ Binary.read_uint32 s 0 in
        let o' = o + 8 + s' in
        match fst $ Binary.read_char8 s 4 with
          | i when i = SerDes.leaf_tag -> loop a o' next_time
          | i when i = SerDes.index_tag -> loop a o' next_time
          | i when i = SerDes.value_tag -> loop a o' next_time
          | i when i = SerDes.commit_tag -> loop o o' next_time
          | c -> failwith
              (Printf.sprintf "Flog.find_root: unknown entry type: %d"
                 (Char.code c))

      with End_of_file ->
        return (a, next_time)
    in

    loop 0 o (Time.zero)

  let extend_file =
    let ks = Posix.fallocate_FALLOC_FL_KEEP_SIZE ()
    and extent = 1024 * 1024 * 512 (* 512 MB *) in

    fun fd offset ->
      S.with_fd fd
        (fun fd ->
          Posix.fallocate fd ks offset extent;
          extent)

  let make (f: string): t S.m =
    let from_some = function
      | Some x -> x
      | _ -> invalid_arg "Flog.make.from_some"
    in

    S.init f >>= fun fd ->
    S.with_fd fd
      (fun fd ->
        Unix.set_close_on_exec fd;
        let cur = Unix.lseek fd 0 Unix.SEEK_CUR in
        lseek_set fd 0;
        Unix.lockf fd Unix.F_TLOCK 0;

        let tbs = Posix.fstat_blksize fd in
        Posix.posix_fadvise fd 0 (2 * tbs) Posix.POSIX_FADV_WILLNEED;

        lseek_set fd cur;

        tbs
      )
    >>= fun tbs ->

      (* TODO Make safe -> close handles! *)
      (* TODO Read metadata *)


    S.read fd 0 tbs >>= fun mds ->
    let md1 = from_some (deserialize_metadata mds) in
    let bs = md1.md_blocksize in
    S.read fd bs bs >>= fun mds ->
    let md2 = from_some (deserialize_metadata mds) in

    S.with_fd fd
      (fun fd -> Posix.posix_fadvise fd 0 (max tbs (2 * bs)) Posix.POSIX_FADV_DONTNEED)
    >>= fun () ->

      (* TODO Use 'best' metadata instead of checking it's value *)
    assert (md1.md_blocksize = md2.md_blocksize);
    assert (md1.md_spindle = md2.md_spindle);

    S.with_fd fd
      (fun fd ->
        let curr = Unix.lseek fd 0 Unix.SEEK_CUR in
        let offset = Unix.lseek fd 0 Unix.SEEK_END in
        lseek_set fd curr;
        offset
      ) >>= fun offset ->

    extend_file fd offset >>= fun extent ->

    let s =
      if md1.md_count > md2.md_count
      then md1.md_offset
      else md2.md_offset
    in
    let s = if s = 0 then (2 * bs) else s in
    find_commit fd s >>= fun (last, now) ->

      (* TODO Choose correct last_metadata *)
      (* TODO Write 'best' metadata into both blocks *)

    let cache = OffsetEntryCache.create 32 in

    return { fd;
             offset;
             commit_offset=last;
             closed=false;
             last_metadata=0; metadata=(md1, md2); space_left=extent;
             d = md1.md_d;
             cache;
             now;
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
    let start = ref t.offset in
    let do_one i e =
      begin
        let s = serialize_entry h e in
        let size = String.length s + String.length marker' in
        let () = Buffer.add_string b marker' in
        let () = Buffer.add_string b s in
          (* TODO Not multi-spindle compatible! *)
        let pos = Outer (0, !start) in
        let () = Hashtbl.replace h i pos in
        let () = start := !start + size in
        ()
      end
    in
    let () = Slab.iteri slab do_one in
    let cp = Hashtbl.find h (sl -1) in
      (* as before *)
    let s = Buffer.contents b in
    let l = String.length s in
    (if t.space_left < l
     then
        extend_file t.fd t.offset >>= fun e ->
     t.space_left <- e;
     return ()
     else
      return ())
    >>= fun () ->

(* TODO Not multi-spindle compatible! *)
    let co = match cp with
      | Inner _ -> failwith "Flog.write: Inner"
      | Outer (0, off) -> off
      | Outer (_,_) -> failwith "Flog.write: Invalid spindle"
    in

    S.append t.fd s 0 l >>= fun _ ->
    t.commit_offset <- co;
    t.offset <- t.offset + l;
    t.space_left <- t.space_left - l;

(* TODO We might want to List.rev slab.es, if slabs get > cache.s big *)
(*
  Slab.iter (fun (p, e) -> OffsetEntryCache.add t.cache p e) slab.es
*)

    return ()

(* TODO Not multi-spindle compatible! *)
  let last t = Outer (0, t.commit_offset)


  let read t pos =
    if pos = Outer (0, 0) then return NIL
    else
      match OffsetEntryCache.get t.cache pos with
        | Some e -> return e
        | None ->
            begin
        (* TODO Not multi-spindle compatible! *)
              let pos' = match pos with
                | Inner _ -> failwith "Flog.read: Inner"
                | Outer (0, off) -> off
                | Outer (_ , _) -> failwith "Flog.read: Invalid spindle"
              in

              S.read t.fd pos' 9 >>= fun s ->

              let m = fst $ Binary.read_uint32 s 0
              and l = fst $ Binary.read_uint32 s 4 in
              assert (m = marker);

              let c = fst $ Binary.read_char8 s 8 in

        (* Special-case values to get-around a useless allocation + substring *)
              if c = SerDes.value_tag
              then
                S.read t.fd (pos' + 4 + 4 + 1 + 1 + 4) (l - 2 - 4 - 4) >>= fun s' ->
              return (Value s')
              else
                S.read t.fd (pos' + 4) (l + 4) >>= fun s' ->

              return $ match c with
                | i when i = SerDes.value_tag -> failwith "Flog.read: value"
                | i when i = SerDes.leaf_tag -> deserialize_leaf s' 0
                | i when i = SerDes.index_tag -> deserialize_index s' 0
                | i when i = SerDes.commit_tag -> deserialize_commit s' 0
                | _ -> failwith "Flog.read: unknown node type"
            end


  let lookup t =
    let p = last t in
    read t p >>= fun e ->
    let c = get_commit e in
    return (Commit.get_lookup c)

  let sync t =
  (* Retrieve current commit offset *)
    let c = t.commit_offset in

  (* Sync file *)
    S.fsync t.fd >>= fun () ->

  (* Figure out which metadata to overwrite *)
    let i = (t.last_metadata + 1) mod 2 in
    let m = (if i = 0 then fst else snd) t.metadata in

  (* Update metadata *)
    let m' = { m with md_offset=c; md_count=(m.md_count + 2) mod 0xFFFFFFFF } in

  (* Write to disk *)
    let s, bs = serialize_metadata m' in
    let o = if i = 0 then 0 else m.md_blocksize in
    S.write t.fd s 0 bs o >>= fun () ->

  (* Sync again *)
    S.with_fd t.fd Posix.fdatasync >>= fun () ->

  (* Update in-memory representation *)
    let md' =
      let a = (if i = 0 then m' else fst t.metadata)
      and b = (if i = 1 then m' else snd t.metadata) in
      (a, b)
    in

    t.metadata <- md';
    t.last_metadata <- i;

    return ()


  let close db =
    if db.closed
    then return ()
    else begin
      S.fsync db.fd >>= fun () ->
      S.with_fd db.fd (fun fd -> Unix.ftruncate fd db.offset )
      >>= fun () ->
      S.close db.fd >>= fun () ->
      db.closed <- true;
      return ()
    end

  let clear (_:t) = failwith "Flog.clear: Not implemented"

(* Hole punching compaction *)
  module OffsetOrder = struct
    type t = offset
    let compare = Pervasives.compare
  end

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
      let n' = n in
      let h' = OffsetSet.max_elt os in
      let h'' = h'  in
      let os' = OffsetSet.remove h' os in

    (* TODO This is not multi-spindle compatible! *)
      read l (Outer (0, h')) >>= fun e ->

      let do_punch =
        if mb = 0 then do_punch_always
        else
          let b' = b / 2 in
          if b' = 4096 then do_punch_4096 else do_punch_generic b'
      in
      let (e', os'') =
        let unpack_offset = function
          | Inner _ -> failwith "Inner"
          | Outer (0, off) -> off
          | Outer (_, _) -> failwith "Non-0 spindle"
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
          | Some fn -> fn (l.offset) i
      in

      cb e';
      S.with_fd l.fd (fun fd -> do_punch fd mb e' (n' - e')) >>= fun () ->

      if OffsetSet.is_empty os''
      then begin
        cb b;
        S.with_fd l.fd (fun fd -> do_punch fd mb b (h'' - b))
      end
      else compact' l pc mb b { cs_offset=h'; cs_entries=os''; }


  let compact ?min_blocks:(mb=0) ?progress_cb:(pc=None) t =
    S.fsync t.fd >>= fun () ->

    let md = (if t.last_metadata = 0 then fst else snd) t.metadata in
    let b = md.md_blocksize in

    let b' = b * 2
    and o = t.commit_offset in

  (* TODO This is not multi-spindle compatible! *)
    read t (Outer (0, o)) >>= function
      | Commit c ->
          let r = Commit.get_pos c in
    (* TODO This is not multi-spindle compatible! *)
          let p = match r with
            | Inner _ -> failwith "Flog.compact: Inner"
            | Outer (0, off) -> off
            | Outer (_, _) -> failwith "Flog.compact: Invalid spindle"
          in
          compact' t pc mb b' { cs_offset=o; cs_entries=OffsetSet.singleton p; }
      | NIL ->
          failwith "Flog.compact: read NIL iso commit entry"
      | Leaf _ | Value _ | Index _ ->
          invalid_arg "Flog.compact: no commit entry at given offset"

  let set_metadata _t _s =
    failwith "not implemented"

  let unset_metadata _t =
    failwith "not implemented"

  let get_metadata _t =
    failwith "not implemented"

end (* module / functor *)
