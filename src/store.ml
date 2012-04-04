(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2012 Incubaid BVBA
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

type offset = int
type string_offset = offset
type store_offset = offset
type length = int

module type STORE =
  sig
    type t
    type 'a m

    val bind : 'a m -> ('a -> 'b m) -> 'b m
    val return : 'a -> 'a m
    val run : 'a m -> 'a

    val init : string -> t m
    val close : t -> unit m

    val next : t -> int

    (* This will result in a string of *at least* the given length *)
    val read : t -> store_offset -> length -> string m
    val write : t -> string -> string_offset -> length -> store_offset -> unit m
    val append : t -> string -> string_offset -> length -> store_offset m

    val fsync : t -> unit m
  end

module Memory : STORE =
  struct
    type t = T of Buffer.t ref
    type 'a m = M of 'a

    (* This is a rather ugly hack *)
    let memory_store = Hashtbl.create 16

    let bind (M v) f = f v
    let return v = M v
    let run (M v) = v

    let init (n:string) =
      if Hashtbl.mem memory_store n
      then
        return (Hashtbl.find memory_store n)
      else
        let v = T (ref (Buffer.create 128)) in
        Hashtbl.replace memory_store n v;
        return v
    let close _ = return ()

    let next (T b) = Buffer.length (!b)

    let read (T b) o l = return (Buffer.sub !b o l)
    let write (T b) d p l o =
      let s = Buffer.contents !b in
      let sl = String.length s in

      let s' =
        if o + l < sl
        then s
        else s ^ (String.create (o + l - sl))
      in

      String.blit d p s' o l;

      let b' = Buffer.create 0 in
      Buffer.add_string b' s';

      b := b';

      return ()

    let append (T b) d p l =
      let o = Buffer.length !b in
      Buffer.add_substring !b d p l;
      return o

    let fsync (T _) = return ()
  end

module Sync : STORE =
  struct
    open Unix

    type t = T of file_descr * int ref

    type 'a m = M of 'a

    let bind (M v) f = f v
    let return v = M v
    let run (M v) = v

    let init name =
      let fd = openfile name [O_RDWR; O_CREAT] 0o640 in
      let stat = fstat fd in
      let len = stat.st_size in

      assert (len > 0);

      let _ = lseek fd len Unix.SEEK_SET in

      return (T (fd, ref len))

    let close (T (fd, _)) =
      Unix.close fd;
      return ()

    let next (T (_, o)) = !o
   
    let read (T (fd, _)) o l =
      let s = String.create l in
      Posix.pread_into_exactly fd s l o;
      return s

    let write (T (fd, _)) d p l o =
      let d' =
        if p = 0
          then d
          else String.sub d p l
      in
      Posix.pwrite_exactly fd d' l o;
      return ()

    let append (T (fd, o)) d p l =
      let rec loop s = function
        | 0 -> !o
        | c ->
            let w = Unix.write fd d s c in
            o := (!o) + w;
            if w = c
              then !o
              else
                loop (s + w) (c - w)
      in
      return (loop p l)

    let fsync (T (fd, _)) =
      Posix.fsync fd;
      return ()
  end

module Lwt_ = Lwt

open Lwt_unix

module Lwt : STORE with type 'a m = 'a Lwt.t =
  struct
    type t = T of Lwt_unix.file_descr * int ref

    type 'a m = 'a Lwt_.t
    let bind = Lwt_.bind
    let return = Lwt_.return
    let run = Lwt_main.run

    let (>>=) = bind

    let init name =
      Lwt_unix.openfile name [Lwt_unix.O_RDWR; Lwt_unix.O_CREAT;] 0o640 >>= fun fd ->
      Lwt_unix.fstat fd >>= fun stat ->
      let len = stat.st_size in

      Lwt_unix.lseek fd len Lwt_unix.SEEK_SET >>= fun i ->
      assert (i = len);

      return (T (fd, ref len))

    let close (T (fd, _)) =
      Lwt_unix.close fd

    let next (T (_, o)) = !o

    let read (T (fd, _)) o l =
      Lwt_unix.lseek fd o Lwt_unix.SEEK_SET >>= fun i ->
      assert (i = o);

      let s = String.create l in

      let rec loop o' = function
        | 0 -> return ()
        | c ->
            Lwt_unix.read fd s o' c >>= fun c' ->
            if c' = 0
            then
              raise End_of_file
            else
              loop (o' + c') (c - c')
      in
      loop 0 l >>= fun () ->

      return s

    let write (T (fd, _)) d p l o =
      Lwt_unix.lseek fd o Lwt_unix.SEEK_SET >>= fun i ->
      assert (i = o);

      let rec loop p' = function
        | 0 -> return ()
        | c ->
            Lwt_unix.write fd d p' c >>= fun c' ->
            if c' = 0
            then
              raise End_of_file
            else
              loop (p' + c') (c - c')
      in
      loop p l

    let append (T (fd, o) as t) d p l =
      let o' = !o in

      write t d p l o' >>= fun () ->
      o := o' + l;
      return o'

    let fsync (T (fd, _)) =
      Lwt_unix.fsync fd
  end
