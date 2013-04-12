open OUnit
open Prefix
open Tree
open Dbx

module MDB = DB(Mlog)
module MDBX = DBX(Mlog)
module MPrefix = Prefix(Mlog)

open Base

let (>>=) = Mlog.bind
let printer r = Pretty.string_of_list (fun s -> s) r

let _ok_set tx k v =
  MDBX.set tx k v >>= fun () ->
  Mlog.return (OK ())

let prefix_keys () =
  let fn = "bla" in
  let () = Mlog.init ~d:2 Time.zero fn in
  let mlog = Mlog.make2 ~n_spindles:1 fn Time.zero in

  let _ =
    MDBX.with_tx mlog
      (fun tx ->
        let rec loop i =
          if i = 16
          then Mlog.return (OK())
          else
            let k = Printf.sprintf "a%03i" i in
            let v = "x" in
            _ok_set tx k v >>= fun (OK()) ->
            loop (i+1)
        in
        loop 0)
  in
  let prefix = "a00" in
  MPrefix.prefix_keys_latest mlog prefix None >>= fun keys ->
  OUnit.assert_equal ~printer
    ["a000";"a001";"a002";"a003";"a004";"a005";
     "a006";"a007";"a008";"a009";]
    keys


let prefix_keys_latest () =
  let log = Mlog.make "mlog" in
  List.iter (fun k -> MDB.set log k (String.uppercase k))
    ["a";"b";"b";"c";"d_0";"d_1"; "d_2";"e";"f";"g"];
  let r = MPrefix.prefix_keys_latest log "d" None in
  OUnit.assert_equal ~printer ["d_0";"d_1";"d_2"] r

let prefix_keys_latest_max () =
  let log = Mlog.make "mlog" in
  List.iter (fun k -> MDB.set log k (String.uppercase k))
    ["a";"b";"b";"c";"d_0";"d_1"; "d_2";"e";"f";"g"];
  let r = MPrefix.prefix_keys_latest log "d" (Some 2) in
  OUnit.assert_equal ~printer ["d_0";"d_1";] r

let suite = "Prefix" >::: [
  "prefix_keys" >:: prefix_keys;
  "prefix_keys_last" >:: prefix_keys_latest;
  "prefix_keys_last_max" >:: prefix_keys_latest_max
]
