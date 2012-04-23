open Catchup
open Mlog
open OUnit
open Dbx

module MCatchup = Catchup(Mlog)
module MDBX = DBX(Mlog)

let (>>=) = Mlog.bind

let catchup1 () = 
  let fn = "mlog" in
  let () = Mlog.init ~d:2 Time.zero fn in
  let mlog = Mlog.make2 ~n_spindles:1 fn Time.zero in
  let rec loop i = 
    if i = 0 then ()
    else
      begin
        Printf.printf "i=%i\n" i;
        Mlog.dump mlog;
        let diff = i mod 2 = 1 in
        let ki = Printf.sprintf "key%i" i in
        let vi = Printf.sprintf "value%i" i in
        MDBX.log_update ~diff mlog (fun tx -> MDBX.set tx ki vi) >>= fun () ->
        loop (i-1)
      end
  in
  loop 11;
  let i0 = 0L in
  let result = MCatchup.catchup i0 (fun acc i actions -> actions :: acc) [] mlog in
  ()
    

let catchup2 () = 
  let fn = "mlog" in
  let () = Mlog.init ~d:2 Time.zero fn in
  let mlog = Mlog.make2 ~n_spindles:1 fn Time.zero in
  MDBX.log_update ~diff:true mlog (fun tx -> MDBX.set tx "xxx" "xxx") >>= fun () ->
  MDBX.commit_last mlog >>= fun () ->
  (* Mlog.dump mlog; *)
  MCatchup.catchup 0L (fun acc i actions -> (actions,i) :: acc) [] mlog >>= fun result ->
  OUnit.assert_equal ~printer:string_of_int 1 (List.length result);
  ()
  
    

let suite = "Catchup" >::: [
  "catchup1" >:: catchup1;
  "catchup2" >:: catchup2;
]
