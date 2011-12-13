let () = 
  let log = Baardskeerder.make "the_log.bs" in
  let () = Baardskeerder.close log in
  ();;
(*
open Flog0

module MyLog = Flog0.Flog0
module MyDBX = DBX(MyLog)

let main() =
  let fn = "the_log.bs" in
  let () = MyLog.init fn Time.zero in
  let log = MyLog.make fn in
  let f tx = 
    let rec loop i = 
      if i = 100 then ()
      else
        let () = MyDBX.set tx (Printf.sprintf "_____key_%i_____" "xxxxx") in
        loop (i+1)
    in
    loop 0
  in
  MyDBX.with_tx log f;
  MyLog.close log;;

*)
