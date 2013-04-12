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

  let iteri_array f a =
    let l = Array.length a in

    let rec loop = function
      | 0 -> return ()
      | n ->
          let i = l - n in
          f i (Array.get a i) >>= fun () ->
          loop (n - 1)
    in
    loop l

  let iter_array f = iteri_array (fun _ v -> f v)

  let init_array n f =
    let rec loop acc = function
      | c when c = n -> return acc
      | c ->
          f c >>= fun v ->
          loop (v :: acc) (c + 1)
    in
    loop [] 0 >>= fun l ->
    let l' = List.rev l in
    return (Array.of_list l')

  let fold_left f acc0 =
    let rec loop acc = function
      | [] -> return acc
      | x :: xs ->
          f acc x >>= fun acc' ->
          loop acc' xs
    in
    loop acc0

end
