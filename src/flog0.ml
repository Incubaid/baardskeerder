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

open Store

open Base
open Entry
open Unix
open Pack



module Flog0 =
  functor(S:Bs_internal.STORE) ->
  struct

type 'a m = 'a S.m
let bind = S.bind
let return = S.return

let (>>=) = bind
module M = Monad.Monad(S)


let time_to b (t:Time.t) =
  let (x,y,g) = t in
  Pack.vint64_to b x;
  Pack.vint_to b y;
  let c = if g then '\x01' else '\x00' in
  Buffer.add_char b c
    

let input_kp input =
  let k = Pack.input_string input in
  let s = Pack.input_vint input in
  let p = Pack.input_vint input in
  k, Outer (Spindle s, Offset p)


let pos_to b = function
  | Outer (Spindle s, Offset o) -> Pack.vint_to b s; Pack.vint_to b o
  | Inner _ -> failwith "cannot serialize inner pos"

let kp_to b (k,p) =
  Pack.string_to b k;
  pos_to b p


let input_time input =
  let x = Pack.input_vint64 input in
  let y = Pack.input_vint input in
  let c = Pack.input_char input in
  let g = match c with 
    | '\x00' -> false
    | '\x01' -> true
    | _ -> failwith "not a bool"
  in
  Time.make x y g


let _METADATA_SIZE = 4096
type metadata = {
  commit : (spindle * offset);
  td: int; 
  t0: Time.t;
}

let metadata2s m =
  let (Spindle s, Offset o) = m.commit in
  Printf.sprintf "{commit=(%i,%i);td=%i;t0=%s}" s o m.td (Time.time2s m.t0)



let _write_metadata fd m = 
  let b = Buffer.create 128 in
  let (Spindle s, Offset o) = m.commit in
  let () = Pack.vint_to b s in
  let () = Pack.vint_to b o in
  let () = Pack.vint_to b m.td in
  let () = time_to b m.t0 in
  let block = String.create _METADATA_SIZE in
  
  let () = Buffer.blit b 0 block 0 (Buffer.length b) in

  S.write fd block 0 _METADATA_SIZE 0

let _read_metadata fd = 
  S.read fd 0 _METADATA_SIZE >>= fun m ->
  let input = Pack.make_input m 0 in
  let s = Pack.input_vint input in
  let o = Pack.input_vint input in
  let commit = (Spindle s, Offset o) in
  let td = Pack.input_vint input in
  let t0 = input_time input in
  return {commit; td;t0}

type t = { spindles : S.t array;
           start : Time.t;
	   mutable last: (spindle * offset);
           mutable next_spindle: int;
	   mutable d: int;
           mutable now: Time.t;
           filename : string;
	 }

let get_d t = t.d

let t2s t =
  let (Spindle ls, Offset lo) = t.last in
  let sp = Array.get t.spindles t.next_spindle in
  Printf.sprintf "{...;last=(%i,%i); next=(%i,%i);now=%s}" ls lo t.next_spindle (S.next sp)
    (Time.time2s t.now)

let last t = let s, o = t.last in Outer (s, o)



let now t = t.now


let close t = 
  let meta = {commit = t.last; td = t.d; t0 = t.start} in
  M.iter_array (fun s -> _write_metadata s meta >>= fun () -> S.close s) t.spindles

let clear t = 
  let commit = (Spindle 0, Offset 0) in
  let meta = {commit;
	      td = t.d;
              t0 = Time.zero;
             } 
  in
  M.iter_array (fun s -> _write_metadata s meta) t.spindles >>= fun () ->
  t.last  <- commit;
  t.next_spindle <- 0;
  t.now  <- Time.zero;

  return ()


type tag = 
  | COMMIT
  | LEAF
  | INDEX
  | VALUE

let tag_to b = function
  | COMMIT -> Buffer.add_char b '\x01'
  | LEAF   -> Buffer.add_char b '\x02'
  | INDEX  -> Buffer.add_char b '\x03'
  | VALUE  -> Buffer.add_char b '\x04'

open Pack
let input_tag input = 
  let tc = input.s.[input.p] in
  let () = input.p <- input.p + 1 in
  match tc with
    | '\x01' -> COMMIT
    | '\x02' -> LEAF
    | '\x03' -> INDEX
    | '\x04' -> VALUE
    | _      -> let s = Printf.sprintf "%C tag?" tc in failwith s


let inflate_action input = 
  let t = input_char input in
  match t with
    | 'D' -> let k = input_string input in
             Commit.CDelete k
    | 'S' -> let k = input_string input in
             let s = input_vint input in
             let p = input_vint input in
             Commit.CSet (k, Outer (Spindle s, Offset p))
    | t   -> let s = Printf.sprintf "%C action?" t in failwith s

       

let inflate_pos input = 
  let s = Pack.input_vint input in
  let p = Pack.input_vint input in
  Outer (Spindle s, Offset p)

let inflate_commit input = 
  let pos = inflate_pos input in
  let previous = inflate_pos input in
  let lookup = inflate_pos input in
  let t = input_time input in
  let actions = Pack.input_list inflate_action input in    
  let explicit = Pack.input_bool input in
  Commit.make_commit ~pos ~previous ~lookup t actions explicit


let inflate_value input = input_string input

let input_suffix_list input =
  let prefix = input_string input in
  let suffixes = input_list input_kp input in
  let kps = List.map (fun (s,p) -> (prefix ^s, p)) suffixes in
  kps

let inflate_leaf input = input_suffix_list input


let inflate_index input = 
  let s0 = input_vint input in
  let p0 = input_vint input in
  let kps = input_suffix_list input in
  Outer (Spindle s0, Offset p0), kps

let input_entry input = 
  match input_tag input with
    | COMMIT -> Commit (inflate_commit input)
    | LEAF   -> Leaf (inflate_leaf input)
    | INDEX  -> Index (inflate_index input)
    | VALUE  -> Value (inflate_value input)

let inflate_entry es = 
  let input = make_input es 0 in
  input_entry input



let dump ?(out=Pervasives.stdout) (t:t) = 
  let d s =
    _read_metadata s >>= fun m ->
    Printf.fprintf out "meta: %s\n%!" (metadata2s m);
    let rec loop pos=
      S.read s pos 4 >>= fun ls ->
      let l = size_from ls 0 in
      S.read s (pos + 4) l >>= fun es ->
      let e = inflate_entry es in
      let () = Printf.fprintf out "%4i : %s\n%!" pos (Entry.entry2s e)  in
      let pos' = pos + 4 + String.length es in
      loop pos'
    in
    try
      loop _METADATA_SIZE
    with End_of_file -> return ()
  in

  M.iteri_array
    (fun i s ->
      Printf.fprintf out "Spindle %02i\n" i;
      Printf.fprintf out "----------\n";
      d s)
    t.spindles


let _add_buffer b mb = 
  let l = Buffer.length mb in
  size_to b l;
  Buffer.add_buffer b mb;
  l + 4

let deflate_value b _ v =
  let l = String.length v in
  let mb = Buffer.create (l+5) in
  tag_to mb VALUE;
  string_to mb v;
  _add_buffer b mb

let pos_remap mb h p = 
  let (s, o) = match p with
    | Outer (Spindle s, Offset o) -> (s, o)
    | Inner x when x = -1-> (0,0) (* TODO: don't special case with -1 *)
    | Inner x ->
      let (Spindle s, Offset o) = Hashtbl.find h x in
      (s, o)
  in
  vint_to mb s;
  vint_to mb o
	       
let kps_to mb h kps = 
  let px = Leaf.shared_prefix kps in
  let pxs = String.length px in
  string_to mb px;
  let l = List.length kps in
  vint_to mb l;
  List.iter (fun (k,p) ->
    let suffix = String.sub k pxs (String.length k - pxs) in
    string_to mb suffix;
    pos_remap mb h p)
    kps


let deflate_index b h (p0, kps) =
  let mb = Buffer.create 128 in
  tag_to mb INDEX;
  pos_remap mb h p0;
  kps_to mb h kps;
  _add_buffer b mb
  
      
let deflate_leaf b h kps = 
  let mb = Buffer.create 128 in
  tag_to mb LEAF;
  kps_to mb h kps;
  _add_buffer b mb


let deflate_action b h = function
  | Commit.CSet (k,p) -> 
    Buffer.add_char b 'S';
    string_to b k;
    pos_remap b h p
  | Commit.CDelete k ->
    Buffer.add_char b 'D';
    string_to b k

let deflate_commit b h c = 
  let mb = Buffer.create 8 in
  tag_to mb COMMIT;
  let pos = Commit.get_pos c in
  let previous = Commit.get_previous c in
  let lookup = Commit.get_lookup c in
  let t = Commit.get_time c in
  
  let actions = Commit.get_cactions c in
  let explicit = Commit.is_explicit c in
  pos_remap mb h pos;
  pos_remap mb h previous;
  pos_remap mb h lookup;
  time_to mb t;
  vint_to mb (List.length actions);
  List.iter (fun a -> deflate_action mb h a) actions;
  bool_to mb explicit;
  _add_buffer b mb

let deflate_entry (b:Buffer.t) h (e:entry) =
  match e with
    | NIL      -> failwith "NIL?"
    | Value v  -> deflate_value  b h v
    | Index i  -> deflate_index  b h i
    | Leaf l   -> deflate_leaf   b h l
    | Commit c -> deflate_commit b h c


let write log slab = 
  let sl = Slab.length slab in
  let h = Hashtbl.create sl in

  let l = Array.length log.spindles in
  let sid = ref log.next_spindle in
  let buffers = Array.init l (fun _ -> Buffer.create 1024) in
  let starts = Array.init l (fun i -> ref (S.next (Array.get log.spindles i))) in

  let do_one i e = 
    let sid' = !sid in
    let start = Array.get starts sid' in

    let size = deflate_entry (Array.get buffers sid') h e in
    let () = Hashtbl.replace h i (Spindle sid', Offset !start) in

    let () = start := !start + size in

    sid := (sid' + 1) mod l
  in
  let () = Slab.iteri slab do_one  in

  M.iteri_array
    (fun i b ->
      let ss = Buffer.contents b
      and s = Array.get log.spindles i
      and n = !(Array.get starts i) in

      S.append s ss 0 (String.length ss) >>= fun _ ->

      assert (n = S.next s);

      return ())
    buffers >>= fun () ->

  let cp = Hashtbl.find h (sl -1) in
  log.last <- cp;
  log.next_spindle <- !sid;
  log.now <- Slab.time slab;
  return ()


let sync t =
  M.iter_array S.fsync t.spindles

let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
  ignore min_blocks;
  ignore progress_cb;
  failwith "todo"

let _read_entry_s fd pos = 
  S.read fd pos 4 >>= fun ls ->
  let l = size_from ls 0 in
  S.read fd (pos + 4) l

let read t pos = 
  match pos with
    | Outer (Spindle s, Offset o) ->
      if ((s = 0) && (o = 0)) then return NIL
      else
	begin
          let sp = Array.get t.spindles s in
	  _read_entry_s sp o >>= fun es ->
	  return (inflate_entry es)
	end
    | Inner _ -> failwith "cannot read inner"


let lookup t = 
  let p = last t in
  read t p >>= function
    | Commit c -> let lu = Commit.get_lookup c in return lu
    | e -> failwith "can only do commits"


let init ?(d=8) fn t0 = 
  S.init fn >>= fun s ->
  if S.next s = 0 then
    let commit = (Spindle 0, Offset 0) in
    _write_metadata s {commit;td = d; t0} >>= fun () ->
    S.close s
  else
    S.close s >>= fun () ->
    let s = Printf.sprintf "%s already exists" fn in
    failwith s


let make filename =
  S.init filename >>= fun s ->
  assert (S.next s > 0);

  let spindles = Array.make 1 s in
  
  let sp0 = Array.get spindles 0 in

  _read_metadata sp0 >>= fun m ->
  let next_free = S.next sp0 in
  let d = m.td in
  let rec _scan_forward lt (tbr:int) = 
    if tbr = next_free then
      S.return lt
    else
      begin
        try
          _read_entry_s sp0 tbr >>= fun es ->
          let input = make_input es 0 in
          let e = input_entry input in
          let next = tbr + 4 + String.length es in
          match e with
            | Commit c -> 
              let time = Commit.get_time c in
              let lt' = (Spindle 0, Offset tbr), time in
              _scan_forward lt'   next
            | e -> _scan_forward lt next
        with End_of_file ->
          S.return lt
      end
  in
  let (_,Offset tbr) = m.commit in
  let corrected_tbr = max _METADATA_SIZE tbr in
  _scan_forward ((Spindle 0, Offset 0),Time.zero) corrected_tbr >>= fun (last, now) ->
  let flog0 = { spindles=spindles;
                last=last;
                next_spindle=0;
                d=d;
                now = now;
                start = m.t0;
                filename;
              }
  in
  return flog0

let set_metadata t s =
  let f = t.filename ^ ".meta" in
  let o = f ^ ".new" in
  let b = Buffer.create 128 in
  size_to b (String.length s);
  Buffer.add_string b s;
  let s' = Buffer.contents b in
  S.init o >>= fun fd ->
  S.write fd s' 0 (String.length s') 0 >>= fun () ->
  S.close fd >>= fun () ->
  Unix.rename o f;
  return ()

let unset_metadata t =
  let () = try
    Unix.unlink (t.filename ^ ".meta")
  with Unix_error (Unix.ENOENT, _, _) -> ()
  in
  return ()

let get_metadata t =
  let f = t.filename ^ ".meta" in
  if
    try
      let _ = Unix.stat f in true
    with Unix_error (Unix.ENOENT, _, _) -> false
  then
    S.init f >>= fun fd ->
    S.read fd 0 4 >>= fun d ->
    let l = size_from d 0 in
    S.read fd 4 l >>= fun s ->
    return (Some s)
  else
    return None

end (* module / functor *)(*
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

open Store

open Base
open Entry
open Unix
open Pack



module Flog0 =
  functor(S:Bs_internal.STORE) ->
  struct

type 'a m = 'a S.m
let bind = S.bind
let return = S.return

let (>>=) = bind
module M = Monad.Monad(S)


let time_to b (t:Time.t) =
  let (x,y,g) = t in
  Pack.vint64_to b x;
  Pack.vint_to b y;
  let c = if g then '\x01' else '\x00' in
  Buffer.add_char b c
    

let input_kp input =
  let k = Pack.input_string input in
  let s = Pack.input_vint input in
  let p = Pack.input_vint input in
  k, Outer (Spindle s, Offset p)


let pos_to b = function
  | Outer (Spindle s, Offset o) -> Pack.vint_to b s; Pack.vint_to b o
  | Inner _ -> failwith "cannot serialize inner pos"

let kp_to b (k,p) =
  Pack.string_to b k;
  pos_to b p


let input_time input =
  let x = Pack.input_vint64 input in
  let y = Pack.input_vint input in
  let c = Pack.input_char input in
  let g = match c with 
    | '\x00' -> false
    | '\x01' -> true
    | _ -> failwith "not a bool"
  in
  Time.make x y g


let _METADATA_SIZE = 4096
type metadata = {
  commit : (spindle * offset);
  td: int; 
  t0: Time.t;
}

let metadata2s m =
  let (Spindle s, Offset o) = m.commit in
  Printf.sprintf "{commit=(%i,%i);td=%i;t0=%s}" s o m.td (Time.time2s m.t0)



let _write_metadata fd m = 
  let b = Buffer.create 128 in
  let (Spindle s, Offset o) = m.commit in
  let () = Pack.vint_to b s in
  let () = Pack.vint_to b o in
  let () = Pack.vint_to b m.td in
  let () = time_to b m.t0 in
  let block = String.create _METADATA_SIZE in
  
  let () = Buffer.blit b 0 block 0 (Buffer.length b) in

  S.write fd block 0 _METADATA_SIZE 0

let _read_metadata fd = 
  S.read fd 0 _METADATA_SIZE >>= fun m ->
  let input = Pack.make_input m 0 in
  let s = Pack.input_vint input in
  let o = Pack.input_vint input in
  let commit = (Spindle s, Offset o) in
  let td = Pack.input_vint input in
  let t0 = input_time input in
  return {commit; td;t0}

type t = { spindles : S.t array;
           start : Time.t;
	   mutable last: (spindle * offset);
           mutable next_spindle: int;
	   mutable d: int;
           mutable now: Time.t;
           filename : string;
	 }

let get_d t = t.d

let t2s t =
  let (Spindle ls, Offset lo) = t.last in
  let sp = Array.get t.spindles t.next_spindle in
  Printf.sprintf "{...;last=(%i,%i); next=(%i,%i);now=%s}" ls lo t.next_spindle (S.next sp)
    (Time.time2s t.now)

let last t = let s, o = t.last in Outer (s, o)



let now t = t.now


let close t = 
  let meta = {commit = t.last; td = t.d; t0 = t.start} in
  M.iter_array (fun s -> _write_metadata s meta >>= fun () -> S.close s) t.spindles

let clear t = 
  let commit = (Spindle 0, Offset 0) in
  let meta = {commit;
	      td = t.d;
              t0 = Time.zero;
             } 
  in
  M.iter_array (fun s -> _write_metadata s meta) t.spindles >>= fun () ->
  t.last  <- commit;
  t.next_spindle <- 0;
  t.now  <- Time.zero;

  return ()


type tag = 
  | COMMIT
  | LEAF
  | INDEX
  | VALUE

let tag_to b = function
  | COMMIT -> Buffer.add_char b '\x01'
  | LEAF   -> Buffer.add_char b '\x02'
  | INDEX  -> Buffer.add_char b '\x03'
  | VALUE  -> Buffer.add_char b '\x04'

open Pack
let input_tag input = 
  let tc = input.s.[input.p] in
  let () = input.p <- input.p + 1 in
  match tc with
    | '\x01' -> COMMIT
    | '\x02' -> LEAF
    | '\x03' -> INDEX
    | '\x04' -> VALUE
    | _      -> let s = Printf.sprintf "%C tag?" tc in failwith s


let inflate_action input = 
  let t = input_char input in
  match t with
    | 'D' -> let k = input_string input in
             Commit.CDelete k
    | 'S' -> let k = input_string input in
             let s = input_vint input in
             let p = input_vint input in
             Commit.CSet (k, Outer (Spindle s, Offset p))
    | t   -> let s = Printf.sprintf "%C action?" t in failwith s

       

let inflate_pos input = 
  let s = Pack.input_vint input in
  let p = Pack.input_vint input in
  Outer (Spindle s, Offset p)

let inflate_commit input = 
  let pos = inflate_pos input in
  let previous = inflate_pos input in
  let lookup = inflate_pos input in
  let t = input_time input in
  let actions = Pack.input_list inflate_action input in    
  let explicit = Pack.input_bool input in
  Commit.make_commit ~pos ~previous ~lookup t actions explicit


let inflate_value input = input_string input

let input_suffix_list input =
  let prefix = input_string input in
  let suffixes = input_list input_kp input in
  let kps = List.map (fun (s,p) -> (prefix ^s, p)) suffixes in
  kps

let inflate_leaf input = input_suffix_list input


let inflate_index input = 
  let s0 = input_vint input in
  let p0 = input_vint input in
  let kps = input_suffix_list input in
  Outer (Spindle s0, Offset p0), kps

let input_entry input = 
  match input_tag input with
    | COMMIT -> Commit (inflate_commit input)
    | LEAF   -> Leaf (inflate_leaf input)
    | INDEX  -> Index (inflate_index input)
    | VALUE  -> Value (inflate_value input)

let inflate_entry es = 
  let input = make_input es 0 in
  input_entry input



let dump ?(out=Pervasives.stdout) (t:t) = 
  let d s =
    _read_metadata s >>= fun m ->
    Printf.fprintf out "meta: %s\n%!" (metadata2s m);
    let rec loop pos=
      S.read s pos 4 >>= fun ls ->
      let l = size_from ls 0 in
      S.read s (pos + 4) l >>= fun es ->
      let e = inflate_entry es in
      let () = Printf.fprintf out "%4i : %s\n%!" pos (Entry.entry2s e)  in
      let pos' = pos + 4 + String.length es in
      loop pos'
    in
    try
      loop _METADATA_SIZE
    with End_of_file -> return ()
  in

  M.iteri_array
    (fun i s ->
      Printf.fprintf out "Spindle %02i\n" i;
      Printf.fprintf out "----------\n";
      d s)
    t.spindles


let _add_buffer b mb = 
  let l = Buffer.length mb in
  size_to b l;
  Buffer.add_buffer b mb;
  l + 4

let deflate_value b _ v =
  let l = String.length v in
  let mb = Buffer.create (l+5) in
  tag_to mb VALUE;
  string_to mb v;
  _add_buffer b mb

let pos_remap mb h p = 
  let (s, o) = match p with
    | Outer (Spindle s, Offset o) -> (s, o)
    | Inner x when x = -1-> (0,0) (* TODO: don't special case with -1 *)
    | Inner x ->
      let (Spindle s, Offset o) = Hashtbl.find h x in
      (s, o)
  in
  vint_to mb s;
  vint_to mb o
	       
let kps_to mb h kps = 
  let px = Leaf.shared_prefix kps in
  let pxs = String.length px in
  string_to mb px;
  let l = List.length kps in
  vint_to mb l;
  List.iter (fun (k,p) ->
    let suffix = String.sub k pxs (String.length k - pxs) in
    string_to mb suffix;
    pos_remap mb h p)
    kps


let deflate_index b h (p0, kps) =
  let mb = Buffer.create 128 in
  tag_to mb INDEX;
  pos_remap mb h p0;
  kps_to mb h kps;
  _add_buffer b mb
  
      
let deflate_leaf b h kps = 
  let mb = Buffer.create 128 in
  tag_to mb LEAF;
  kps_to mb h kps;
  _add_buffer b mb


let deflate_action b h = function
  | Commit.CSet (k,p) -> 
    Buffer.add_char b 'S';
    string_to b k;
    pos_remap b h p
  | Commit.CDelete k ->
    Buffer.add_char b 'D';
    string_to b k

let deflate_commit b h c = 
  let mb = Buffer.create 8 in
  tag_to mb COMMIT;
  let pos = Commit.get_pos c in
  let previous = Commit.get_previous c in
  let lookup = Commit.get_lookup c in
  let t = Commit.get_time c in
  
  let actions = Commit.get_cactions c in
  let explicit = Commit.is_explicit c in
  pos_remap mb h pos;
  pos_remap mb h previous;
  pos_remap mb h lookup;
  time_to mb t;
  vint_to mb (List.length actions);
  List.iter (fun a -> deflate_action mb h a) actions;
  bool_to mb explicit;
  _add_buffer b mb

let deflate_entry (b:Buffer.t) h (e:entry) =
  match e with
    | NIL      -> failwith "NIL?"
    | Value v  -> deflate_value  b h v
    | Index i  -> deflate_index  b h i
    | Leaf l   -> deflate_leaf   b h l
    | Commit c -> deflate_commit b h c


let write log slab = 
  let sl = Slab.length slab in
  let h = Hashtbl.create sl in

  let l = Array.length log.spindles in
  let sid = ref log.next_spindle in
  let buffers = Array.init l (fun _ -> Buffer.create 1024) in
  let starts = Array.init l (fun i -> ref (S.next (Array.get log.spindles i))) in

  let do_one i e = 
    let sid' = !sid in
    let start = Array.get starts sid' in

    let size = deflate_entry (Array.get buffers sid') h e in
    let () = Hashtbl.replace h i (Spindle sid', Offset !start) in

    let () = start := !start + size in

    sid := (sid' + 1) mod l
  in
  let () = Slab.iteri slab do_one  in

  M.iteri_array
    (fun i b ->
      let ss = Buffer.contents b
      and s = Array.get log.spindles i
      and n = !(Array.get starts i) in

      S.append s ss 0 (String.length ss) >>= fun _ ->

      assert (n = S.next s);

      return ())
    buffers >>= fun () ->

  let cp = Hashtbl.find h (sl -1) in
  log.last <- cp;
  log.next_spindle <- !sid;
  log.now <- Slab.time slab;
  return ()


let sync t =
  M.iter_array S.fsync t.spindles

let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
  ignore min_blocks;
  ignore progress_cb;
  failwith "todo"

let _read_entry_s fd pos = 
  S.read fd pos 4 >>= fun ls ->
  let l = size_from ls 0 in
  S.read fd (pos + 4) l

let read t pos = 
  match pos with
    | Outer (Spindle s, Offset o) ->
      if ((s = 0) && (o = 0)) then return NIL
      else
	begin
          let sp = Array.get t.spindles s in
	  _read_entry_s sp o >>= fun es ->
	  return (inflate_entry es)
	end
    | Inner _ -> failwith "cannot read inner"


let lookup t = 
  let p = last t in
  read t p >>= function
    | Commit c -> let lu = Commit.get_lookup c in return lu
    | e -> failwith "can only do commits"


let init ?(d=8) fn t0 = 
  S.init fn >>= fun s ->
  if S.next s = 0 then
    let commit = (Spindle 0, Offset 0) in
    _write_metadata s {commit;td = d; t0} >>= fun () ->
    S.close s
  else
    S.close s >>= fun () ->
    let s = Printf.sprintf "%s already exists" fn in
    failwith s


let make filename =
  S.init filename >>= fun s ->
  assert (S.next s > 0);

  let spindles = Array.make 1 s in
  
  let sp0 = Array.get spindles 0 in

  _read_metadata sp0 >>= fun m ->
  let next_free = S.next sp0 in
  let d = m.td in
  let rec _scan_forward lt (tbr:int) = 
    if tbr = next_free then
      S.return lt
    else
      begin
        try
          _read_entry_s sp0 tbr >>= fun es ->
          let input = make_input es 0 in
          let e = input_entry input in
          let next = tbr + 4 + String.length es in
          match e with
            | Commit c -> 
              let time = Commit.get_time c in
              let lt' = (Spindle 0, Offset tbr), time in
              _scan_forward lt'   next
            | e -> _scan_forward lt next
        with End_of_file ->
          S.return lt
      end
  in
  let (_,Offset tbr) = m.commit in
  let corrected_tbr = max _METADATA_SIZE tbr in
  _scan_forward ((Spindle 0, Offset 0),Time.zero) corrected_tbr >>= fun (last, now) ->
  let flog0 = { spindles=spindles;
                last=last;
                next_spindle=0;
                d=d;
                now = now;
                start = m.t0;
                filename;
              }
  in
  return flog0

let set_metadata t s =
  let f = t.filename ^ ".meta" in
  let o = f ^ ".new" in
  let b = Buffer.create 128 in
  size_to b (String.length s);
  Buffer.add_string b s;
  let s' = Buffer.contents b in
  S.init o >>= fun fd ->
  S.write fd s' 0 (String.length s') 0 >>= fun () ->
  S.close fd >>= fun () ->
  Unix.rename o f;
  return ()

let unset_metadata t =
  let () = try
    Unix.unlink (t.filename ^ ".meta")
  with Unix_error (Unix.ENOENT, _, _) -> ()
  in
  return ()

let get_metadata t =
  let f = t.filename ^ ".meta" in
  if
    try
      let _ = Unix.stat f in true
    with Unix_error (Unix.ENOENT, _, _) -> false
  then
    S.init f >>= fun fd ->
    S.read fd 0 4 >>= fun d ->
    let l = size_from d 0 in
    S.read fd 4 l >>= fun s ->
    return (Some s)
  else
    return None

end (* module / functor *)



