(* pread *)
external pread_into_exactly: Unix.file_descr -> string -> int -> int -> unit
  = "_bs_posix_pread_into_exactly"
(* pwrite *)
external pwrite_exactly: Unix.file_descr -> string -> int -> int -> unit
  = "_bs_posix_pwrite_exactly"

(* fsync *)
external fsync: Unix.file_descr -> unit
  = "_bs_posix_fsync"
(* fdatasync *)
external fdatasync: Unix.file_descr -> unit
  = "_bs_posix_fdatasync"

(* fallocate *)
external fallocate_FALLOC_FL_KEEP_SIZE: unit -> int
  = "_bs_posix_fallocate_FALLOC_FL_KEEP_SIZE"
external fallocate_FALLOC_FL_PUNCH_HOLE: unit -> int
  = "_bs_posix_fallocate_FALLOC_FL_PUNCH_HOLE"
external fallocate: Unix.file_descr -> int -> int -> int -> unit
  = "_bs_posix_fallocate"

(* posix_fadvise *)
type posix_fadv = POSIX_FADV_NORMAL
                  | POSIX_FADV_SEQUENTIAL
                  | POSIX_FADV_RANDOM
                  | POSIX_FADV_NOREUSE
                  | POSIX_FADV_WILLNEED
                  | POSIX_FADV_DONTNEED

external posix_fadvise: Unix.file_descr -> int -> int -> posix_fadv -> unit
  = "_bs_posix_fadvise"

(* stat blksize *)
external fstat_blksize: Unix.file_descr -> int
  = "_bs_posix_fstat_blksize"

(* fiemap ioctl *)
external ioctl_fiemap: Unix.file_descr -> (int64 * int64 * int64 * int32) list
  = "_bs_posix_ioctl_fiemap"
