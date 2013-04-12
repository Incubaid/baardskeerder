open Tree
open OUnit

module MDB = DB(Mlog)

let setup () =
  let log = Mlog.make "mlog" in
  List.iter
    (fun k -> MDB.set log k (String.uppercase k))
    ["a";"b";"c";"d";"e";"f";"g";"h";];
  log

let teardown _ = ()

let key_count_simple log =
  let c = MDB.key_count log in
  OUnit.assert_equal ~printer:string_of_int 8 c

let wrap t = OUnit.bracket setup t teardown

let suite = "KeyCount" >::: [
  "key_count_simple" >:: wrap key_count_simple
]
