open Log
open Entry

module Prefix = functor (L:LOG ) -> struct
  let prefix_keys (t:L.t) (slab: Slab.t) prefix max = 
    let prefix_ok k = 
      let i_end = min (String.length k)  (String.length prefix) in
      let rec loop i =
        if i = i_end
        then true
        else k.[i] = prefix.[i] && loop (i+1)
      in
      loop 0
    in
    let (>>= ) = L.bind in
    let return = L.return in
    let t_max = 
      match max with 
        | None -> fun _ -> true
        | Some m -> (>) m
    in
    let lp = 
      if Slab.is_empty slab 
      then L.last t 
      else Slab.last slab 
    in
    let _read t pos = match pos with
      | Pos.Inner _ -> return (Slab.read slab pos)
      | Pos.Outer _ -> L.read t pos
    in
    let rec walk_leaf count acc leaf = 
      let rec loop count acc = function
        | [] -> 
          return (count,acc)
        | (k, vpos) :: t ->
          if t_max count 
          then
            let ok = prefix_ok k in
            let count', acc'  = 
              if ok
              then (count+1), k :: acc 
              else count    , acc
            in
            loop count' acc' t
          else
            return (count,acc)
      in
      loop count acc leaf
    and walk_index (count:int) (acc: Base.k list) (p,kps) =
      let rec loop count acc p kps = 
        match kps with
          | [] -> walk count acc p
          | (k,pk) :: t ->
            begin
              if t_max count 
              then
                if prefix_ok k 
                then 
                  begin
                    walk count acc p >>= fun (count', acc') ->
                    loop count' acc' pk t
                  end
                else
                  loop count acc pk t
              else
                return (count,acc)
            end
      in
      loop count acc p kps
    and walk count acc pos = 
      _read t pos >>= function
        | NIL       -> return (count, acc)
        | Value _   -> return (count, acc)
        | Leaf leaf ->  walk_leaf count acc leaf
        | Index index -> walk_index count acc index
        | Commit _  -> failwith "prefix_keys walk : unexpected commit entry"          
    in
    _read t lp >>= fun e ->
    match e with
      | NIL -> return []
      | Commit c -> 
        let root = Commit.get_lookup c in
        begin
          walk 0 [] root >>= fun (_,r) -> 
          return (List.rev r)
        end
      | Index (p,kps) -> walk_index 0 [] (p,kps) >>= fun (_,r) -> return (List.rev r)
      | Leaf leaf  ->  walk_leaf 0 [] leaf       >>= fun (_,r) -> return (List.rev r)
      | Value _ -> failwith "did not expect Value here"

  let prefix_keys_latest t prefix max = 
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    prefix_keys t slab prefix max
end
