open OUnit
open Flog0

module MyF = Flog0
module MyLog = MyF(Store.Sync)
module MyDB = Tree.DB(MyLog)
module MySync = Sync.Sync(MyLog)
module MyInspect_storage = Inspect_storage.Inspect_storage(MyF)(Store.Sync)

let test_inspect_storage () =
  let (>>=) = MyLog.bind in
  let fn = "test_inspect_storage.db" in
  let t =
    MyLog.init fn Time.zero >>= fun () ->
    MyLog.make fn >>= fun log ->
    let keysizes = [1; 4; 256; 2048; 65535; 65539; 67108864; 4294967296] in
    let rec loop i = function
      | [] -> MyLog.sync log
      | keysize::t ->
          MyDB.set log ("key" ^ (string_of_int i)) (String.create keysize) >>= fun () ->
          loop (i + 1) t
    in
    loop 0 keysizes >>= fun () ->
    MyInspect_storage.inspect_storage log >>= fun storage ->
    let buckets = MyInspect_storage.bucketize storage (fun vs _ -> vs) 1 in
    let (_, vs, va, _, _) = List.hd buckets in
    OUnit.assert_equal ~msg:"sizes not equal" vs (List.fold_left (+) 0 keysizes);
    OUnit.assert_equal ~msg:"number of keys not equal" va (List.length keysizes) in
  Store.Sync.run t;
  Unix.unlink fn

let suite =
  "Inspect_storage" >::: [
    "inspect_storage" >:: test_inspect_storage;
  ]
