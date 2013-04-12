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

open Lwt
open Lwt_unix

external stub_pread : Unix.file_descr -> string -> int -> int -> int -> int =
    "lwt_unix_ext_pread"
external pread_job : Unix.file_descr -> int -> int -> [ `unix_pread ] job =
    "lwt_unix_ext_pread_job"
external pread_result : [ `unix_pread ] job -> string -> int -> int =
    "lwt_unix_ext_pread_result"
external pread_free : [ `unix_pread ] job -> unit =
    "lwt_unix_ext_pread_free" "noalloc"

let pread ch buf pos len offset =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix_ext.pread"
  else
    let ch' = Lwt_unix.unix_file_descr ch in
    (*Lwt_unix.blocking ch >>= function
      | true ->
      Lwt_unix.wait_read ch >>= fun () ->
      let job =  pread_job ch' len offset in
      let result =  (fun job -> pread_result job buf pos) in
      let  free = pread_free in
    (* let async_method = Lwt_unix.Async_switch in *)
      Lwt_unix.execute_job
    (* ~async_method (* SEGVs *) *)
      job
      result
      free
      | false ->
      Lwt.fail (Failure "not here") >>= fun () ->
    *)
    wrap_syscall Lwt_unix.Read ch
      (fun () -> stub_pread ch' buf pos len offset)


external stub_pwrite : Unix.file_descr -> string -> int -> int -> int -> int =
        "lwt_unix_ext_pwrite"
external pwrite_job : Unix.file_descr -> string -> int -> int -> int -> [ `unix_pwrite ] job =
        "lwt_unix_ext_pwrite_job"
external pwrite_result : [ `unix_pwrite ] job -> int =
        "lwt_unix_ext_pwrite_result"
external pwrite_free : [ `unix_pwrite ] job -> unit =
        "lwt_unix_ext_pwrite_free" "noalloc"

let pwrite ch buf pos len offset =
  if pos < 0 || len < 0 || pos > String.length buf - len then
    invalid_arg "Lwt_unix_ext.pwrite"
  else
    Lwt_unix.blocking ch >>= function
      | true ->
          Lwt_unix.wait_write ch >>= fun () ->
          Lwt_unix.execute_job
            (pwrite_job (Lwt_unix.unix_file_descr ch) buf pos len offset)
            pwrite_result
            pwrite_free
      | false ->
          wrap_syscall Lwt_unix.Write ch
            (fun () -> stub_pwrite (Lwt_unix.unix_file_descr ch) buf pos len offset)
