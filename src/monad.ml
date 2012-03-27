module type MONAD = sig
  type 'a m

  val bind : 'a m -> ('a -> 'b m) -> 'b m
  val return : 'a -> 'a m
end

module Monad(M:MONAD) = struct
  let (>>=) = M.bind
  and return = M.return

  let iter f =
    let rec loop = function
      | [] -> return ()
      | x :: xs ->
          f x >>= fun () ->
          loop xs
    in
    loop

  let fold_left f acc0 =
    let rec loop acc = function
      | [] -> return acc
      | x :: xs ->
          f acc x >>= fun acc' ->
          loop acc' xs
    in
    loop acc0
       
end
