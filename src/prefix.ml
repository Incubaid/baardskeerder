open Log
open Entry

module Prefix = functor (L:LOG ) -> struct
  let prefix_keys (t:L.t) prefix max = 
    let (>>= ) = L.bind in
    let return = L.return in
    let lp = L.last t in
    L.read t lp >>= function
      | NIL -> return []
      | Commit c ->
        let root = Commit.get_lookup c in
        begin
          let t_max count = match
              max with
                | None -> true
                | Some m -> count < m
          in
          let prefix_ok k = 
            let i_end = min (String.length k)  (String.length prefix) in
            let rec loop i =
              if i = i_end
              then true
              else k.[i] = prefix.[i] && loop (i+1)
            in
            loop 0
          in
          let rec walk count acc pos = 
            L.read t pos >>= function
              | NIL       -> return (count, acc)
              | Value _   -> return (count, acc)
              | Leaf leaf ->  walk_leaf count acc leaf
              | Index index -> walk_index count acc index
              | Commit _  -> failwith "prefix_keys walk : unexpected commit entry"
          and walk_leaf count acc leaf = 
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
          and walk_index count acc (p,kps) =
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
          in
          walk 0 [] root >>= fun (_,r) -> 
          return r
        end
      | Index _ | Leaf _ | Value _ -> failwith "not expected entry type"
end
