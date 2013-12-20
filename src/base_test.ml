let ok_or_fail = function
  | Base.OK () -> Mlog.return ()
  | Base.NOK _ -> failwith "NOK"
