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

val pread_into_exactly: Unix.file_descr -> string -> int -> int -> unit
val pwrite_exactly: Unix.file_descr -> string -> int -> int -> unit
val fsync: Unix.file_descr -> unit
val fdatasync: Unix.file_descr -> unit


val fallocate_FALLOC_FL_KEEP_SIZE: unit -> int 
val fallocate_FALLOC_FL_PUNCH_HOLE: unit -> int
val fallocate: Unix.file_descr -> int -> int -> int -> unit

type posix_fadv = POSIX_FADV_NORMAL
                  | POSIX_FADV_SEQUENTIAL
                  | POSIX_FADV_RANDOM
                  | POSIX_FADV_NOREUSE
                  | POSIX_FADV_WILLNEED
                  | POSIX_FADV_DONTNEED

val  posix_fadvise: Unix.file_descr -> int -> int -> posix_fadv -> unit
val fstat_blksize: Unix.file_descr -> int  

val ioctl_fiemap: Unix.file_descr -> (int64 * int64 * int64 * int32) list
