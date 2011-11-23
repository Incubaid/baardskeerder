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
  let (<<) = Int32.shift_left in
  let (>>) = Int32.shift_right_logical in
  let char_at n =
    let pos = n * 8 in
    let mask = Int32.of_int 0xff << pos in
    let code = (Int32.logand i32 mask)  >> pos in
    Char.chr (Int32.to_int code)
  in
  let add i = Buffer.add_char b (char_at i) in
  add 0;
  add 1;
  add 2;
  add 3


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
  k, Outer p


let pos_to b = function
  | Outer p -> vint_to b p
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
  


let _METADATA_SIZE = 4096
type metadata = {commit : int; 
		 td: int }

let metadata2s m = Printf.sprintf "{commit=%i;td=%i}" m.commit m.td



let _write_metadata fd m = 
  let b = Buffer.create 128 in
  let () = vint_to b m.commit in
  let () = vint_to b m.td in
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
  {commit; td}

type t = { fd : file_descr; 
	   mutable last: int; 
	   mutable next:int;
	   mutable d: int;
	 }

let get_d t = t.d

let t2s t = Printf.sprintf "{...;last=%i; next=%i}" t.last t.next

let next t = Outer t.next
let last t = Outer t.last



let close t = 
  let meta = {commit = t.last; td = t.d} in
  let () = _write_metadata t.fd meta in
  Unix.close t.fd

let clear t = 
  let commit = 0 in
  let meta = {commit;
	      td = t.d } 
  in
  _write_metadata t.fd meta;
  t.last  <- commit;
  t.next <- _METADATA_SIZE


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
    | _ -> let s = Printf.sprintf "%C tag?" tc in failwith s


let inflate_commit input = 
  let p = input_vint input in
  Commit (Outer p)

let inflate_value input = Value (input_string input)

let inflate_leaf input = Leaf (input_list input_kp input)

let inflate_index input = 
  let p0 = input_vint input in
  let kps = input_list input_kp input in
  Index (Outer p0,kps)

let input_entry input = 
  match input_tag input with
    | COMMIT -> inflate_commit input
    | LEAF   -> inflate_leaf input
    | INDEX  -> inflate_index input
    | VALUE  -> inflate_value input

let inflate_entry es = 
  let input = make_input es 0 in
  input_entry input



let dump ?(out=Pervasives.stdout) (t:t) = 
  _seek t.fd 0 ;
  let m = _read_metadata t.fd in
  Printf.fprintf out "meta: %s\n" (metadata2s m);
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
    | Outer x -> x
    | Inner x -> 
      let o = Hashtbl.find h x in
      o
  in
  vint_to mb o
	       
let deflate_index b h (p0, kps) =
  let mb = Buffer.create 128 in
  tag_to mb INDEX;
  pos_remap mb h p0;
  let l = List.length kps in
  vint_to mb l;
  List.iter (fun (k,p) -> 
    string_to mb k; 
    pos_remap mb h p) 
    kps;
  _add_buffer b mb
  
      
let deflate_leaf b h kps = 
  let mb = Buffer.create 128 in
  tag_to mb LEAF;
  let l = List.length kps in
  vint_to mb l;
  List.iter (fun (k,p) ->
    string_to mb k;
    pos_remap mb h p)
    kps;
  _add_buffer b mb

let deflate_commit b h p = 
  let mb = Buffer.create 8 in
  tag_to mb COMMIT;
  pos_remap mb h p;
  _add_buffer b mb

let deflate_entry (b:Buffer.t) h (e:entry) =
  match e with
    | NIL -> failwith "NIL?"
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
  ()


let sync t = Posix.fsync t.fd

let _read_entry_s fd pos = 
  let ls = _seek_read fd pos 4 in
  let l = size_from ls 0 in
  let es = _really_read fd l in
  es 

let read t pos = 
  match pos with
    | Outer p ->
      if p = 0 then NIL
      else
	begin
	  let es = _read_entry_s t.fd p in
	  inflate_entry es
	end
    | Inner _ -> failwith "cannot read inner"


let init ?(d=4) fn = 
  let fd = openfile fn [O_CREAT;O_WRONLY;] 0o640 in
  let stat = fstat fd in
  let len = stat.st_size in
  if len = 0 then
    let commit = 0 in
    let () = _write_metadata fd {commit;td = d} in   
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
  let next = 
    if last = 0 
    then _METADATA_SIZE 
    else 
      let s = _read_entry_s fd last in
      last + 4 + String.length s 
  in
  {fd ; last; next; d}



