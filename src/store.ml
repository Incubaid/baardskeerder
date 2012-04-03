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
    type t = T of string ref
    type 'a m = M of 'a

    let bind (M v) f = f v
    let return v = M v
    let run (M v) = v

    let init (_:string) = return (T (ref ""))
    let close _ = return ()

    let next (T s) = String.length (!s)

    let read (T s) o l = return (String.sub !s o l)
    let write (T s) d p l o = failwith "Not implemented"
    let append (T s) d p l =
      let d' = String.sub d p l
      and o = String.length (!s) in
      s := ((!s) ^ d');
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
      let fd = openfile name [O_RDWR] 0o640 in
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
