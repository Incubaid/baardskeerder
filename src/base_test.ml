let ok_or_fail = function
  | Ok () -> Mlog.return ()
  | Error _ -> failwith "Error"
