open Catchup
open Mlog
open OUnit
open Dbx

module MCatchup = Catchup(Mlog)
module MDBX = DBX(Mlog)

let t_catchup1 () = 
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
        Mlog.bind 
          (MDBX.log_update ~diff mlog (fun tx -> MDBX.set tx ki vi))
          (fun () -> loop (i-1))
      end
  in
  loop 11;
  let t0 = Time.zero in
  let result = MCatchup.catchup t0 (fun acc actions -> actions :: acc) [] mlog in
  ()
    

let suite = "Catchup" >::: [
  "catchup1" >:: t_catchup1;
]
