open Lwt
open OUnit

open Tree
open Test_helper

module MDB = DB(Mlog_lwt)

let with_db fn () =
  Lwt_main.run (
    Mlog_lwt.make "" >>= fun db ->
    fn db
  )

let test_set db =
  MDB.set db "key" "value"

let test_set_get db =
  MDB.set db "key" "value" >>= fun () ->
  MDB.get db "key" >>= fun vo ->
  OUnit.assert_equal ~printer:vo2s vo (Some "value");
  return ()

let basic =
  "basic" >::: [
    "test_set" >:: with_db test_set;
    "test_set_get" >:: with_db test_set_get;
  ]

let suite =
  "Mlog_lwt" >::: [
    basic;
  ]
