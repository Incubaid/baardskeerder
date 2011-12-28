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

open Base
open Entry
open Unix


let _really_write fd string = 
  let l = String.length string in
  let rec loop start = function
    | 0 -> ()
    | n -> let bw = Unix.write fd string start n in
	   if bw = n then ()
	   else
	     let start' = start + bw in
	     let n' = n - bw in
	     loop start' n' 
  in loop 0 l

let _really_read fd n = 
  let r = String.create n in
  let rec loop start = function
    | 0 -> r
    | n -> let br = Unix.read fd r start n in
	   if br = 0 then raise End_of_file
	   else
	     if br = n 
	     then r
	     else
	       let start' = start + br in
	       let n' = n - br in
	       loop start' n' 
  in
  loop 0 n

let _seek fd pos = let _ = Unix.lseek fd pos Unix.SEEK_SET in ()
    
let _seek_read fd pos n = 
  _seek fd pos;
  _really_read fd n
 
let _seek_write fd pos s = 
  let () = _seek fd pos in
  let () = _really_write fd s in
  ()

let vint_to b n = 
  let add c = Buffer.add_char b c in
  let rec loop = function
    | 0 -> add '\x00'
    | n when n < 128 -> add (Char.chr n)
    | n -> let byte = (n land 0x7f) lor 0x80  in
	   let () = add (Char.chr byte) in
	   let r = n lsr 7 in
	   loop r
  in loop n

let time_to b (x,y,g) = 
  vint_to b x;
  vint_to b y;
  let c = if g then '\x01' else '\x00' in
  Buffer.add_char b c
    
    

let string_to b s = 
  let l = String.length s in
  vint_to b l;
  Buffer.add_string b s


type input = {s:string; mutable p:int}  

let make_input s p = {s;p}

let input2s input = Printf.sprintf "{%S;%i}" input.s input.p

let size_from s pos =
  let byte_of i= Char.code s.[pos + i] in
  let b0 = byte_of 0
  and b1 = byte_of 1
  and b2 = byte_of 2
  and b3 = byte_of 3 in
  let result = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24)
  in result

let size_to b (p:int) = 
  let i32  = Int32.of_int p in
  let (<<<) = Int32.shift_left in
  let (>>>) = Int32.shift_right_logical in
  let char_at n =
    let pos = n * 8 in
    let mask = (Int32.of_int 0xff) <<< pos in
    let code = (Int32.logand i32 mask) >>> pos in
    Char.chr (Int32.to_int code)
  in
  let add i = Buffer.add_char b (char_at i) in
  add 0;
  add 1;
  add 2;
  add 3


let input_char input = 
  let c = input.s.[input.p] in
  let () = input.p <- input.p + 1 in
  c


let input_vint input = 
  let s = input.s in
  let start = input.p in
  let rec loop v f p = 
    let c = s.[p] in
    let cv = Char.code c in
    if cv < 0x80 
    then 
      let () = input.p <- p+ 1  in
      v + cv * f
    else 
      let v' = v + (cv land 0x7f) * f in
      loop v' (f * 128) (p+1)
  in loop 0 1 start

let input_size input = 
  let p = input.p in
  let () = input.p <- p + 4 in
  size_from input.s p

let input_string input = 
  let l = input_vint input in
  let s = String.sub input.s input.p l in
  let () = input.p <- input.p + l in
  s

let input_kp input =
  let k = input_string input in
  let p = input_vint input in
  k, outer0 (Offset p)


let pos_to b = function
  | Outer _ as o -> let p = from_outer0 o in vint_to b p
  | Inner _ -> failwith "cannot serialize inner pos"

let kp_to b (k,p) =
  string_to b k;
  pos_to b p

let input_list input_e input = 
  let l = input_vint input in
  let rec loop acc = function
    | 0 -> List.rev acc
    | n -> let e = input_e input in
	   loop (e :: acc) (n-1)
  in
  loop [] l

let list_to b e_to list = 
  let l = Leaf.length list in
  vint_to b l;
  List.iter (e_to b) list

let input_time input =
  let x = input_vint input in
  let y = input_vint input in
  let c = input_char input in
  let g = match c with 
    | '\x00' -> false
    | '\x01' -> true
    | _ -> failwith "not a bool"
  in
  Time.make x y g


let _METADATA_SIZE = 4096
type metadata = {
  commit : int; 
  td: int; 
  t0: Time.t;
}

let metadata2s m = Printf.sprintf "{commit=%i;td=%i;t0=%s}" m.commit m.td (Time.time2s m.t0)



let _write_metadata fd m = 
  let b = Buffer.create 128 in
  let () = vint_to b m.commit in
  let () = vint_to b m.td in
  let () = time_to b m.t0 in
  let block = String.create _METADATA_SIZE in
  
  let () = Buffer.blit b 0 block 0 (Buffer.length b) in
  _seek fd 0;
  _really_write fd block

let _read_metadata fd = 
  _seek fd 0;
  let m = _really_read fd _METADATA_SIZE in
  let input = make_input m 0 in
  let commit = input_vint input in
  let td = input_vint input in
  let t0 = input_time input in
  {commit; td;t0}

type t = { fd : file_descr; 
           start : Time.t;
	   mutable last: int; 
	   mutable next:int;
	   mutable d: int;
           mutable now: Time.t;
           
	 }

let get_d t = t.d

let t2s t = Printf.sprintf "{...;last=%i; next=%i;now=%s}" t.last t.next (Time.time2s t.now)

let last t = outer0 (Offset t.last)

let now t = t.now


let close t = 
  let meta = {commit = t.last; td = t.d; t0 = t.start} in
  let () = _write_metadata t.fd meta in
  Unix.close t.fd

let clear t = 
  let commit = 0 in
  let meta = {commit;
	      td = t.d;
              t0 = Time.zero;
             } 
  in
  _write_metadata t.fd meta;
  t.last  <- commit;
  t.next <- _METADATA_SIZE;
  t.now  <- Time.zero


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
             Commit.Delete k
    | 'S' -> let k = input_string input in
             let p = input_vint input in
             Commit.Set (k, outer0 (Offset p))
    | t   -> let s = Printf.sprintf "%C action?" t in failwith s

       



let inflate_commit input = 
  let p = input_vint input in
  let prev = input_vint input in
  let t = input_time input in
  let actions = input_list inflate_action input in    
  Commit.make_commit (outer0 (Offset p)) (outer0 (Offset prev)) t actions


let inflate_value input = input_string input

let input_suffix_list input =
  let prefix = input_string input in
  let suffixes = input_list input_kp input in
  let kps = List.map (fun (s,p) -> (prefix ^s, p)) suffixes in
  kps

let inflate_leaf input = input_suffix_list input


let inflate_index input = 
  let p0 = input_vint input in
  let kps = input_suffix_list input in
  outer0 (Offset p0), kps

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
  _seek t.fd 0 ;
  let m = _read_metadata t.fd in
  Printf.fprintf out "meta: %s\n%!" (metadata2s m);
  let rec loop pos= 
    let ls = _really_read t.fd 4 in
    let l = size_from ls 0 in
    let es = _really_read t.fd l in
    let e = inflate_entry es in
    let () = Printf.fprintf out "%4i : %s\n%!" pos (Entry.entry2s e)  in
    let pos' = pos + 4 + String.length es in
    loop pos'
  in
  try
    loop _METADATA_SIZE
  with End_of_file -> ()

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
  let o = match p with
    | Outer _ as o -> from_outer0 o
    | Inner x -> 
      let o = Hashtbl.find h x in
      o
  in
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
  | Commit.Set (k,p) -> 
    Buffer.add_char b 'S';
    string_to b k;
    pos_remap b h p
  | Commit.Delete k ->
    Buffer.add_char b 'D';
    string_to b k

let deflate_commit b h c = 
  let mb = Buffer.create 8 in
  tag_to mb COMMIT;
  let p = Commit.get_pos c in
  let t = Commit.get_time c in
  let prev = Commit.get_previous c in
  let actions = Commit.get_actions c in
  pos_remap mb h p;
  pos_remap mb h prev;
  time_to mb t;
  vint_to mb (List.length actions);
  List.iter (fun a -> deflate_action mb h a) actions;
  _add_buffer b mb

let deflate_entry (b:Buffer.t) h (e:entry) =
  match e with
    | NIL      -> failwith "NIL?"
    | Value v  -> deflate_value  b h v
    | Index i  -> deflate_index  b h i
    | Leaf l   -> deflate_leaf   b h l
    | Commit c -> deflate_commit b h c


let write log slab = 
  let b = Buffer.create 1024 in
  let sl = Slab.length slab in
  let h = Hashtbl.create sl in
  let start = ref log.next in
  let do_one i e = 
    let size = deflate_entry b h e in
    let () = Hashtbl.replace h i !start in
    let () = start := !start + size in
    ()
  in
  let () = Slab.iteri slab do_one  in
  let cp = Hashtbl.find h (sl -1) in
  let ss = Buffer.contents b in
  let () = _seek_write log.fd log.next ss in
  log.last <- cp;
  log.next <- log.next + String.length ss;
  log.now <- Slab.time slab;
  ()


let sync t = Posix.fsync t.fd

let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) =
  ignore min_blocks;
  ignore progress_cb;
  failwith "todo"

let _read_entry_s fd pos = 
  let ls = _seek_read fd pos 4 in
  let l = size_from ls 0 in
  let es = _really_read fd l in
  es 

let read t pos = 
  match pos with
    | Outer _ as o ->
      let p = from_outer0 o in
      if p = 0 then NIL
      else
	begin
	  let es = _read_entry_s t.fd p in
	  inflate_entry es
	end
    | Inner _ -> failwith "cannot read inner"


let init ?(d=4) fn t0 = 
  let fd = openfile fn [O_CREAT;O_WRONLY;] 0o640 in
  let stat = fstat fd in
  let len = stat.st_size in
  if len = 0 then
    let commit = 0 in
    let () = _write_metadata fd {commit;td = d; t0} in   
    let () = Unix.close fd in
    ()
  else
    let () = Unix.close fd in
    let s = Printf.sprintf "%s already exists" fn in
    failwith s


let make filename = 
  let fd = openfile filename [O_RDWR] 0o640 in
  let m = _read_metadata fd in
  let last = m.commit in
  let d = m.td in
  let (next:int),(now:Time.t) = 
    if last = 0 
    then 
      _METADATA_SIZE , m.t0
    else 
      let s = _read_entry_s fd last in
      let input = make_input s 0 in
      let e = input_entry input in
      match e with
        | Commit c ->
          let now = Commit.get_time c in
          (last + 4 + String.length s) , now
        | NIL | Value _ | Index _ | Leaf _ -> 
          let msg = Printf.sprintf "%s should have been a commit" (entry2s e) in failwith msg
  in
  let flog0 = {fd =fd ; last = last; 
               next = next; 
               d=d; 
               now = now;
               start = m.t0;
              } 
  in
  flog0



