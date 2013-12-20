open Catchup
open Mlog
open OUnit
open Dbx

module MCatchup = Catchup(Mlog)
module MDBX = DBX(Mlog)

let (>>=) = Mlog.bind

let ok_set tx ki vi = MDBX.set tx ki vi >>= fun () -> return (Base.OK ())

let ok_unit x = match x with
  | Base.OK () -> Mlog.return ()
  | Base.NOK _ -> failwith "should not happen"

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
        MDBX.log_update ~diff mlog (fun tx -> ok_set tx ki vi) >>= ok_unit >>= fun () ->
        loop (i-1)
      end
  in
  loop 11;
  let i0 = 0L in
  let result = MCatchup.catchup i0 (fun acc _ actions -> actions :: acc) [] mlog in
  ignore result


let catchup2 () =
  let fn = "mlog" in
  let () = Mlog.init ~d:2 Time.zero fn in
  let mlog = Mlog.make2 ~n_spindles:1 fn Time.zero in
  MDBX.log_update ~diff:true mlog (fun tx -> ok_set tx "xxx" "xxx") >>= ok_unit >>= fun () ->
  MDBX.commit_last mlog >>= fun () ->
  (* Mlog.dump mlog; *)
  MCatchup.catchup 0L (fun acc i actions -> (actions,i) :: acc) [] mlog >>= fun result ->
  OUnit.assert_equal ~printer:string_of_int 1 (List.length result);
  ()

let catchup_doubles () =
  let fn = "mlog" in
  let () = Mlog.init ~d:2 Time.zero fn in
  let mlog = Mlog.make2 ~n_spindles:1 fn Time.zero in
  MDBX.log_update ~diff:true  mlog (fun tx -> ok_set tx "1" "1") >>= ok_unit >>= fun () ->
  MDBX.log_update ~diff:true  mlog (fun tx -> ok_set tx "2" "2") >>= ok_unit >>= fun () ->
  MDBX.log_update ~diff:false mlog (fun tx -> ok_set tx "2'" "2'") >>= ok_unit >>= fun () ->
  MDBX.log_update ~diff:true  mlog (fun tx -> ok_set tx "3" "3") >>= ok_unit >>= fun () ->
  MDBX.log_update ~diff:true  mlog (fun tx -> ok_set tx "4" "4") >>= ok_unit >>= fun () ->
  MCatchup.catchup 0L (fun acc i actions -> (actions,i)::acc) [] mlog >>= fun result ->
  let rresult = List.rev result in
  let action2s = function
    | Base.Set(k,v) -> Printf.sprintf "SET(%S,%S)" k v
    | Base.Delete k -> Printf.sprintf "DELETE(%S)" k
  in
  print_newline();
  List.iter (fun (al,i) ->
    let als = String.concat ";" (List.map action2s al) in
    Printf.printf "%Li:%s\n" i als
  ) rresult;
  OUnit.assert_equal ~printer:string_of_int 3 (List.length rresult)

let suite = "Catchup" >::: [
  "catchup1" >:: catchup1;
  "catchup2" >:: catchup2;
  "catchup_doubles" >:: catchup_doubles
]
