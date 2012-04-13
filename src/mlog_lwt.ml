type t = Mlog.t

type 'a m = 'a Lwt.t
let bind = Lwt.bind
let return = Lwt.return

let wrap1 f = fun a -> return (f a)
let wrap2 f = fun a b -> return (f a b)

let init ?(d=2) a b = return (Mlog.init ~d:d a b)
let make = wrap1 Mlog.make

let write = wrap2 Mlog.write
let last = Mlog.last
let lookup = wrap1 Mlog.lookup
let read = wrap2 Mlog.read
let sync = wrap1 Mlog.sync
let close = wrap1 Mlog.close
let clear = wrap1 Mlog.clear
let get_d = Mlog.get_d
let now = Mlog.now

let dump ?out:out_channel _ = failwith "Not implemented"
let compact ?(min_blocks=1) ?(progress_cb=None) (_:t) = failwith "Not implemented"

let set_metadata = wrap2 Mlog.set_metadata
let get_metadata = wrap1 Mlog.get_metadata
let unset_metadata = wrap1 Mlog.unset_metadata
