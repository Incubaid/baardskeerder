(*
 * This file is part of Baardskeerder.
 *
 * Copyright (C) 2011 Incubaid BVBA
 *
 * Baardskeerder is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Baardskeerder is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Baardskeerder.  If not, see <http://www.gnu.org/licenses/>.
 *)

(*
  ..
*)
open Log
open Entry
open Base
open Leaf
open Index
open Slab

module DB = functor (L:LOG ) -> struct

  let (>>=) = L.bind
  and return = L.return

  let _get (t:L.t) (slab:Slab.t) (k:k) =
    let _read pos = match pos with
      | Outer _ -> L.read t pos
      | Inner _ -> return (Slab.read slab pos)
    in
    let rec descend pos =
      _read pos >>= fun e ->
      match e with
        | NIL -> return (NOK k)
        | Value v -> return (OK v)
        | Leaf l -> descend_leaf l
        | Index i -> descend_index i
        | Commit _ -> let msg = Printf.sprintf "descend reached a second commit %s" (Pos.pos2s pos) in
                      failwith msg
    and descend_leaf = function
      | [] -> return (NOK k)
      | (k0,p0) :: t ->
          if k= k0 then descend p0 else
            if k > k0 then descend_leaf t
            else return (NOK k)
    and descend_index (p0, kps) =
      let rec loop pi = function
        | []                       -> pi
        | (ki,_) :: _ when k <= ki -> pi
        | (_ ,p) :: t              -> loop p t
      in
      let pos' = loop p0 kps in
      descend pos'
    in
    let rec descend_root () =
      if Slab.is_empty slab
      then
        let pos = L.last t in
        L.read t pos >>= fun e ->
        match e with
          | Commit c -> descend (Commit.get_lookup c)
          | NIL -> return (NOK k)
          | Index _ | Leaf _ | Value _ -> failwith "descend_root does not start at appropriate level"
      else
        let pos = Slab.last slab in
        descend pos
    in
    descend_root ()

  let get t k =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    _get t slab k

  let rec _set_descend (t:L.t) slab (k:k) pos trail =
    begin
      match pos with
        | Inner _ -> return (Slab.read slab pos)
        | Outer _ -> L.read t pos
    end
    >>= function
      | NIL     -> return []
      | Value _ -> failwith "value ?"
      | Leaf l  -> return (descend_leaf k trail l)
      | Index i -> descend_index t slab k trail i
      | Commit _-> failwith "commit ?"
  and descend_leaf k trail leaf =
    let z = leaf_find_set leaf k in
    Leaf_down z :: trail
  and descend_index t slab k trail index =
    let z = Indexz.find_set index k in
    let trail' = Index_down z :: trail in
    let pos' = Indexz.pos z in
    _set_descend t slab k pos' trail'

  let _set_descend_root t slab k =
    if Slab.is_empty slab
    then
      let pos = L.last t in
      L.read t pos >>= fun e ->
      match e with
        | NIL -> return []
        | Commit c -> let lookup = Commit.get_lookup c in _set_descend t slab k lookup []
        | Index _ | Value _ | Leaf _ ->
            let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in failwith s
    else
      let pos = Slab.last slab in
      _set_descend t slab k pos []


  let _set (t:L.t) slab k v =
    let d = L.get_d t in
    let rec set_start slab start trail =
      match trail with
        | [] -> let vpos = Slab.add_value slab v in
                let lpos = Slab.add_leaf  slab [k,vpos] in
                lpos
        | Leaf_down z :: rest ->
            if Leafz.max d z
            then
              let left, (sep,_) , right = Leafz.split d k start z in
              let _    = Slab.add_value slab v     in
              let lpos = Slab.add_leaf  slab left  in
              let rpos = Slab.add_leaf  slab right in
              set_overflow slab lpos sep rpos rest
            else
              let l = Leafz.insert k start z in
              let _    = Slab.add_value slab v in
              let lpos = Slab.add_leaf  slab l in
              set_rest slab lpos rest
        | Index_down _ :: _ -> failwith "trail must start with Leaf_down _"
    and set_rest (slab:Slab.t) start = function
      | [] -> start
      | (Index_down z) :: rest ->
          let index = Indexz.replace start z in
          let ipos = Slab.add_index slab index    in
          set_rest slab ipos rest
      | Leaf_down _ :: _ -> failwith "rest of trail cannot contain Leaf_down _ "
    and set_overflow (slab:Slab.t) lpos sep rpos trail =
      match trail with
        | [] -> Slab.add_index slab  (lpos, [sep,rpos])
        | Index_down z :: rest ->
            if Indexz.max d z
            then
              let left, sep', right  = Indexz.split d lpos sep rpos z in
              let lpos' = Slab.add_index slab left  in
              let rpos' = Slab.add_index slab right in
              set_overflow slab lpos' sep' rpos' rest
            else
              let z' = Indexz.insert lpos sep rpos z in
              let i' = Indexz.close z' in
              let start' = Slab.add_index slab i' in
              set_rest slab start' rest
        | Leaf_down _ :: _ -> failwith "rest of trail cannot contain Leaf_down _ "
    in
    _set_descend_root t slab k >>= fun trail ->
    let next = Slab.next slab in
    return (set_start slab next trail)


  let set (t:L.t) k v =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    _set t slab k v >>= fun (pos:pos) ->
    let caction = Commit.CSet (k,Inner 0) in (* a little knowledge is a dangerous thing *)
    let previous = L.last t in
    let lookup = pos in
    let commit = Commit.make_commit ~pos ~previous ~lookup fut [caction] false in (* UGLY *)
    let _ = Slab.add_commit slab commit in
    L.write t slab


  let rec _delete_rest slab start (lr : k option) trail = match trail with
    | [] -> return start
    | Index_down z :: rest ->
        begin
          let _step slab lr z rest =
            let index = Indexz.replace start z in
            let ipos = Slab.add_index slab index in
            _delete_rest slab ipos lr rest
          in
          match lr with
            | None -> _step slab lr z rest
            | Some sep ->
                if Indexz.can_go_right z
                then
                  let z' = Indexz.replace_right sep z in
                  let lr' = None in (* since there is a larger subtree, I don't know the lr anymore *)
                  _step slab lr' z' rest
                else
                  _step slab lr z rest
        end
    | Leaf_down _ :: _ -> failwith "rest trail cannot contain Leaf_down _ "

  let _delete (t:L.t) slab k =
    let d = L.get_d t in
    let _read pos = match pos with
      | Inner _ -> return (Slab.read slab pos)
      | Outer _ -> L.read t pos
    in
    let rec descend pos trail =
      _read pos >>= function
        | NIL -> failwith "corrupt: read a NIL node"
        | Value _ -> return (Some trail)
        | Leaf l -> descend_leaf trail l
        | Index i -> descend_index trail i
        | Commit _ -> let msg = Printf.sprintf "descend reached a second commit %s" (Pos.pos2s pos) in
                      failwith msg
    and descend_leaf trail leaf =
      match Leafz.find_delete leaf k with
        | None -> return None
        | Some (p,z) ->
            let step = Leaf_down z in
            descend p (step::trail)
    and descend_index trail index =
      let z = Indexz.find_set index k in
      let trail' = Index_down z :: trail in
      let pos' = Indexz.pos z in
      descend pos' trail'
    and delete_start slab start trail =
      match trail with
        | [] -> failwith "empty trail impossible?"
        | [Leaf_down z ]->
            let leaf', _ = Leafz.delete z in
            return (Slab.add_leaf slab leaf')
        | Leaf_down z :: rest ->
            if Leafz.min d z
            then leaf_underflow slab z rest
            else
              let leaf',lr = Leafz.delete z in
              let _ = Slab.add_leaf slab leaf' in
          (* let () = Printf.printf "lpos = %s <-> start = %s\n" (pos2s lpos) (pos2s start) in *)
              _delete_rest slab start lr rest
        | Index_down _ :: _ -> failwith "trail cannot start with Index_down _"

    and leaf_underflow slab leafz rest : Pos.pos L.m =
      match rest with
        | [] ->
            let leaf',_ = Leafz.delete leafz in
            let (rp:pos) = Slab.add_leaf slab leaf' in
            return rp
        | Index_down z :: rest ->
            begin
              let read_leaf pos =
                _read pos >>= function
                  | Leaf l -> return l
                  | Index _ | Value _ | Commit _ | NIL -> failwith "should be leaf"
              in
              let nb = Indexz.neighbours z in
              match nb with
                | Indexz.NR pos     ->
                    begin
                      read_leaf pos >>= fun right ->
                      if leaf_min d right
                      then
                        begin
                          let left, _  = Leafz.delete leafz in
                          let h,sep_c  =  leaf_merge left right in
                          let hpos = Slab.add_leaf slab h in
                          let z' = Indexz.suppress Indexz.R hpos sep_c z in
                          let sep_c' = if Indexz.can_go_right z' then None else sep_c in
                          let index' = Indexz.close z' in
                          xxx_merged slab hpos sep_c' index' rest
                        end
                      else (* borrow from right *)
                        begin
                          let left, _ = Leafz.delete leafz in
                          let left', _, right' = leaf_borrow_right left right in
                          let sep_c = leaf_min_key right in
                          let lpos = Slab.add_leaf slab left' in
                          let rpos = Slab.add_leaf slab right' in
                          xxx_borrowed_right slab lpos rpos z sep_c rest
                        end
                    end
                | Indexz.NL pos ->
                    begin
                      read_leaf pos >>= fun left ->
                      if leaf_min d left
                      then
                        begin
                          let right, _ = Leafz.delete leafz in
                          let h, sep_c = leaf_merge left right  in
                          let (hpos:pos) = Slab.add_leaf slab h in
                          let z' = Indexz.suppress Indexz.L hpos sep_c z in
                          let index' = Indexz.close z' in
                          xxx_merged slab hpos sep_c index' rest >>= fun (rp:_) ->
                          return rp
                        end
                      else (* borrow from left *)
                        begin
                          let right, sep_c = Leafz.delete leafz in
                          let left', sep', right' = leaf_borrow_left left right in
                          let lpos = Slab.add_leaf slab left' in
                          let rpos = Slab.add_leaf slab right' in
                          xxx_borrowed_left slab lpos sep' rpos z sep_c rest
                        end
                    end
                | Indexz.N2 (p0,p1) ->
                    begin
                      read_leaf p0 >>= fun left ->
                      read_leaf p1 >>= fun right ->
                      match (leaf_mergeable d left, leaf_mergeable d right) with
                        | true,_ ->
                            begin
                              let right, _ = Leafz.delete leafz in
                              let h, lr = leaf_merge left right in
                              let hpos = Slab.add_leaf slab h in
                              let z' = Indexz.suppress Indexz.L hpos lr z in
                              let index' = Indexz.close z' in
                              let lr'  =
                                if Indexz.can_go_right z
                                then None
                                else lr
                              in
                              xxx_merged slab hpos lr' index' rest
                            end
                        | _, true ->
                            begin
                              let left,_ = Leafz.delete leafz in
                              let h, lr = leaf_merge left right in
                              let hpos = Slab.add_leaf slab h in
                              let z' = Indexz.suppress Indexz.R hpos lr z in
                              let index' = Indexz.close z' in
                              let lr' =
                                if Indexz.can_go_right z'
                                then None
                                else lr
                              in
                              xxx_merged slab hpos lr' index' rest
                            end
                        | _,_ -> (* borrow from left *)
                            begin
                              let right, sep_c = Leafz.delete leafz in
                              let left', sep', right' = leaf_borrow_left left right in
                              let lpos = Slab.add_leaf slab left' in
                              let rpos = Slab.add_leaf slab right' in
                              xxx_borrowed_left slab lpos sep' rpos z sep_c rest
                            end
                    end
            end
        | Leaf_down _ :: _  -> failwith "rest of trail cannot contain Leaf_down"
    and xxx_borrowed_right slab lpos rpos  z (lr:k) rest =
      let z' = Indexz.borrowed_right lpos lr rpos z in
      let lr' = if Indexz.can_go_right z' then None else Some lr in
      let index' = Indexz.close z' in
      let ipos = Slab.add_index slab index' in
      _delete_rest slab ipos lr' rest
    and xxx_borrowed_left slab (lpos:pos) sep (rpos:pos) z lr rest =
      match lr with
        | None ->
            let z2 = Indexz.borrowed_left lpos sep rpos z in
            let index' = Indexz.close z2 in
            let ipos' = Slab.add_index slab index' in
            _delete_rest slab ipos' None rest
        | Some s ->
            let z2 =
              if Indexz.can_go_right z
              then Indexz.replace_right s z
              else z
            in
            let z3 = Indexz.borrowed_left lpos sep rpos z2 in
            let lr' = if Indexz.can_go_right z3 then None else lr in
            let index' = Indexz.close z3 in
            let ipos' = Slab.add_index slab index' in
            _delete_rest slab ipos' lr' rest
    and xxx_merged slab (start:pos) sep_c (index:Index.index) rest : Pos.pos L.m =
      let read_index pos =
        _read pos >>= function
          | Index i -> return i
          | Value _ | Leaf _ | Commit _ | NIL -> failwith "should be index"
      in
      let merge_left left index z rest : Pos.pos L.m =
        begin
          let sep = Indexz.separator Indexz.L z in
          let index1 = index_merge left sep index in
          let ipos1 = Slab.add_index slab index1 in
          let z2 = Indexz.suppress Indexz.L ipos1 sep_c z in
          let lr' = if Indexz.can_go_right z2 then None else sep_c in
          let index2 = Indexz.close z2 in
          xxx_merged slab ipos1 lr' index2 rest
        end
      in
      let merge_right right index z lr rest : Pos.pos L.m =
        begin
          match lr with
            | None ->
                let sep = Indexz.separator Indexz.R z in
                let index' = index_merge index sep right in
                let ipos' = Slab.add_index slab index' in
                let z2 = Indexz.suppress Indexz.R ipos' sep_c z in
                let sep_c = None in (* after the merge, we're clueless *)
                let index2 = Indexz.close z2 in
                xxx_merged slab ipos' sep_c index2 rest
            | Some s ->
                let index' = index_merge index s right in
                let ipos' = Slab.add_index slab index' in
                let z2 = Indexz.suppress Indexz.R ipos' lr z in
                let sep_c = None in
                let index2 = Indexz.close z2 in
                xxx_merged slab ipos' sep_c index2 rest
        end
      in
      match index, rest with
        | (_,[]) , []  -> return (start:pos)
        | index , [] -> return (Slab.add_index slab index)
        | index , Index_down z :: rest when index_below_min d index ->
            begin
              let nb = Indexz.neighbours z in
              match nb with
                | Indexz.NL pos ->
                    begin
                      read_index pos >>= fun left ->
                      if index_mergeable d left
                      then merge_left left index z rest
                      else (* borrow from left *)
                        let psep = Indexz.separator Indexz.L z in
                        let left',sep',right' = index_borrow_left left psep index in
                        let lpos' = Slab.add_index slab left' in
                        let rpos' = Slab.add_index slab right' in
                        xxx_borrowed_left slab lpos' sep' rpos' z sep_c rest
                    end

                | Indexz.NR pos ->
                    begin
                      read_index pos >>= fun right ->
                      if index_mergeable d right
                      then merge_right right index z sep_c rest
                      else
                        let sep = match sep_c with
                          | None -> Indexz.separator Indexz.R z
                          | Some s -> s
                        in
                        let left', right' = index_borrow_right index (Some sep) right in
                        let lpos = Slab.add_index slab left' in
                        let rpos = Slab.add_index slab right' in
                        let lr' = index_min_key right in
                        xxx_borrowed_right slab lpos rpos z lr' rest
                    end

                | Indexz.N2 (pl,pr) ->
                    read_index pl >>= fun left ->
                    read_index pr >>= fun right ->
                    match (index_mergeable d left,index_mergeable d right) with
                      | true,_ -> merge_left left index z rest
                      | _, true -> merge_right right index z sep_c rest
                      | _,_ -> (* be consistent with leaf strategy: borrow from left *)
                          begin
                            let psep = Indexz.separator Indexz.L z in
                            let left', sep', right' = index_borrow_left left psep index in
                            let lpos' = Slab.add_index slab left' in
                            let rpos' = Slab.add_index slab right' in
                            xxx_borrowed_left slab lpos' sep' rpos' z sep_c rest
                          end

            end
        | _ -> let ipos = Slab.add_index slab index in
               _delete_rest slab ipos sep_c rest
    in
    let descend_root () =
      if Slab.is_empty slab
      then
        let lp = L.last t in
        L.read t lp >>= fun e ->
        match e with
          | NIL -> return None
          | Commit c -> let lookup = Commit.get_lookup c in descend lookup []
          | Index _ | Leaf _ | Value _  ->
              let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in
              failwith s
      else
        descend (Slab.last slab) []
    in
    descend_root () >>= fun trail_o ->
    match trail_o with
      | None       -> return (NOK k)
      | Some trail -> let start = Slab.next slab in
                      delete_start slab start trail >>= fun (rp':pos) ->
                      return (OK rp')


  let delete (t:L.t) k =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    _delete t slab k >>= function
      | OK (pos:pos) ->
          let caction = Commit.CDelete k in
          let previous = L.last t in
          let lookup = pos in
          let commit = Commit.make_commit ~pos ~previous ~lookup fut [caction] false in
          let _ = Slab.add_commit slab commit in
          L.write t slab >>= fun () ->
          return (OK ())
      | NOK k -> return (NOK k)


  let _range t
      (first: k option) finc
      (last: k option) linc
      (max: int option) (f:k -> pos -> unit L.m) =

    let lp = L.last t in
    L.read t lp >>= function
      | NIL  -> return 0
      | Commit c ->
          begin
            let lookup = Commit.get_lookup c in
            let root = lookup in
            let t_left k = match first with
              | None -> true
              | Some k_f -> if finc then k_f <= k else k_f < k
            and t_right k = match last with
              | None -> true
              | Some k_l -> if linc then k <= k_l else k < k_l
            and ti_left k = match first with
              | None -> true
              | Some k_f -> k_f <= k
            and ti_right k = match last with
              | None -> true
              | Some k_l -> k <= k_l
            and t_max count = match
                max with
                  | None -> true
                  | Some m -> count < m
            in
            let rec walk count pos =
              L.read t pos >>= function
                | NIL     -> return count
                | Value _ -> return count
                | Leaf leaf -> walk_leaf count leaf
                | Index index -> walk_index count index
                | Commit c -> let lookup = Commit.get_lookup c in walk count lookup
            and walk_leaf count leaf =
              let rec loop count = function
                | [] -> return count
                | (k,vpos) :: t ->
                    if t_max count
                    then
                      begin
                        if t_left k
                        then
                          if t_right k
                          then
                            f k vpos >>= fun () ->
                        loop (count + 1) t
                          else return count
                        else
                          loop count t
                      end
                    else return count
              in
              loop count leaf
            and walk_index count (p,kps) =
              let rec loop count p  = function
                | [] -> walk count p
                | (k,pk) :: t when ti_left k ->
                    begin
                      if t_max count
                      then walk count p >>= fun count' ->
                      if ti_right k
                      then loop count' pk t
                      else return count'
                      else return count
                    end
                | (k,pk) :: t ->
                    begin
                      if ti_right k
                      then
                        walk count p >>= fun count' ->
                      if ti_right k && t_max count'
                      then loop count' pk t
                      else return count'
                      else
                        return count
                    end
              in
              loop count p kps
            in
            walk 0 root
          end
      | Index _ | Leaf _ | Value _ -> failwith "not expected entry type"

  let range (t:L.t) (first:k option) (finc:bool) (last:k option) (linc:bool) (max:int option) =
    let acc = ref [] in
    let f k vpos =
      let () = acc := k :: !acc in
      return ()
    in
    _range t first finc last linc max f >>= fun _ ->
    return (List.rev !acc)

  let range_entries (t:L.t) (first: k option) (finc:bool) (last:k option) (linc:bool) (max:int option) =
    let acc = ref [] in
    let f k vpos =
      L.read t vpos >>= function
        | Value v -> let () = acc := (k,v)::!acc in return ()
        | _ -> failwith "should be value"
    in
    _range t first finc last linc max f >>= fun _ ->
    return (List.rev !acc)

  let _fold_reverse_range_while t
      (first: k option) finc
      (last: k option) linc
      (f: 'a -> Base.k -> Base.pos -> (bool * 'a) L.m )
      (is: 'a) =

    let lp = L.last t in
    L.read t lp >>= function
      | NIL  -> return is
      | Commit c ->
          begin
            let left_of_range k =
              match last with
                | None -> false
                | Some f -> if linc then k < f else k <= f
            and right_of_range k =
              match first with
                | None -> false
                | Some f -> if finc then k > f else k >= f
            in

            let rec walk s pos: (bool * 'a) L.m =
              L.read t pos >>= function
                | NIL -> failwith "Tree._fold_reverse_range_while: unexpected entry NIL"
                | Value _ -> failwith "Tree._fold_reverse_range_while: unexpected entry Value"
                | Leaf leaf -> walk_leaf s leaf
                | Index index -> walk_index s index
                | Commit c -> failwith "Tree._fold_reverse_range_while: unexpected entry Commit"
            and walk_leaf s leaf =
              let rec loop s' = function
                | [] -> return (true, s')
                | (k, _) :: _ when left_of_range k ->
                    return (false, s')
                | (k, _) :: kps when right_of_range k ->
                    loop s' kps
                | (k, p) :: kps ->
                    f s' k p >>= fun (cont, s'') ->
                    if cont
                    then
                      loop s'' kps
                    else
                      return (false, s'')
              in
              loop s (List.rev leaf)
            and walk_index s (p, kps) =
              let rec loop s' = function
                | [] -> walk s' p
                | (k, p') :: kps when left_of_range k ->
              (* Need to check one index entry left of the lowest 'valid'
               * entry, since it might point to some more valid keys *)
                    walk s' p'
                | (k, p') :: kps when right_of_range k ->
                    loop s' kps
                | (k, p') :: kps ->
                    walk s' p' >>= fun (cont, s'') ->
                    if cont
                    then
                      loop s'' kps
                    else
                      return (false, s'')
              in
              loop s (List.rev kps)
            in

            walk is (Commit.get_pos c) >>= fun (_, r) ->
            return r
          end
      | Index _ | Leaf _ | Value _ ->
          failwith "Tree._fold_reverse_range_while: unexpected entry type"

  let reverse_range (t:L.t)
      (first:k option) (finc:bool)
      (last:k option) (linc:bool)
      (max:int option) =

    let f (count, acc) k _ = begin
      let count' = count + 1 in

      let cont = match max with
        | None -> true
        | Some c -> count' < c
      in

      return (cont, (count', k :: acc))
    end in

    _fold_reverse_range_while t first finc last linc f (0, []) >>= fun (_, res) ->
    return (List.rev res)

  let rev_range_entries (t:L.t)
      (first:k option) (finc:bool)
      (last: k option) (linc:bool)
      (max: int option) =
    let f (count, acc) k o =
      begin
        let count' = count + 1 in
        let cont = match max with
          | None -> true
          | Some c -> count' < c
        in
        L.read t o >>= fun entry ->
        let v = match entry with
          | Entry.Value v -> v
          | _ -> failwith "Not a value"
        in
        return (cont, (count', (k,v)::acc))
      end
    in
    _fold_reverse_range_while t first finc last linc f (0, []) >>= fun (_, res) ->
    return (List.rev res)

  let confirm (t:L.t) (s:Slab.t) k v =
    let set_needed () =
      _get t s k >>= function
        | NOK _   -> return true
        | OK vc   -> return (vc <> v)
    in
    set_needed () >>= fun sn ->
    if sn
    then _set t s k v >>= fun _ -> return ()
    else return ()


  let depth (t:L.t) (slab:Slab.t )=
    let rec _depth_descend t slab pos c = begin
      match pos with
        | Inner _ -> return (Slab.read slab pos)
        | Outer _ -> L.read t pos
    end
                                          >>= function
                                            | NIL -> return c
                                            | Value _ -> return c
                                            | Leaf l ->
                                                begin
                                                  match l with
                                                    | [] -> return c
                                                    | (_,p) :: _ -> _depth_descend t slab p (c+1)
                                                end
                                            | Index (p0,_) -> _depth_descend t slab p0 (c+1)
                                            | Commit _ -> failwith "reached a second commit on descend"
    in
    let _depth_descend_root t slab =
      if Slab.is_empty slab
      then
        let pos = L.last t in
        L.read t pos >>= fun e ->
        match e with
          | NIL -> return 0
          | Commit c -> let lookup = Commit.get_lookup c in _depth_descend t slab lookup 1
          | Index _ | Value _ | Leaf _ ->
              let s = Printf.sprintf "did not expect:%s" (Entry.entry2s e) in failwith s
      else
        let pos = Slab.last slab in
        _depth_descend t slab pos 1
    in
    _depth_descend_root t slab

  let _key_count (t:L.t) (slab:Slab.t) =
    let foldl f acc0 es =
      let rec _foldl acc0 = function
        | [] -> return acc0
        | e0 :: es -> f acc0 e0 >>= fun acc -> _foldl acc es
      in
      _foldl acc0 es
    in
    let rec _kc_descend t slab pos c =
      begin
        match pos with
          | Inner _ -> return (Slab.read slab pos)
          | Outer _ -> L.read t pos
      end >>= function
        | NIL     -> return c
        | Value _ -> return c
        | Leaf l  -> return (c + List.length l)
        | Index (p0, kps) ->
            _kc_descend t slab p0 c >>= fun c' ->
            foldl (fun acc (k,p) -> _kc_descend t slab p acc) c' kps
        | Commit _ -> failwith "reaced a second commit on descend"
    in
    let _kc_descend_root t slab =
      if Slab.is_empty slab
      then
        let pos = L.last t in
        L.read t pos >>= fun e ->
        match e with
          | NIL -> return 0
          | Commit c -> let lookup = Commit.get_lookup c in _kc_descend t slab lookup 0
          | Index _ | Value _ | Leaf _ -> failwith (Printf.sprintf "did not expect:%s" (Entry.entry2s e))
      else
        let pos = Slab.last slab in
        _kc_descend t slab pos 0
    in
    _kc_descend_root t slab

  let key_count t =
    let now = L.now t in
    let fut = Time.next_major now in
    let slab = Slab.make fut in
    _key_count t slab

end
