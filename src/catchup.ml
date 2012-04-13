open Log
open Commit
open Entry
open Time
open Base

module Catchup(L: LOG) = struct
  type action = 
    |Set of k * v
    |Delete of k

  let catchup (start:Time.t) (f : 'a -> action list -> 'a L.m) (a0:'a) (log : L.t) =
    let (>>=) = L.bind in
    let _read_value pos =
      (L.read log pos) >>= (function 
        | Value v -> L.return v
        | e -> failwith (Printf.sprintf "Catchup:%s is not a value" (entry2s e))
       )
    in
    let _translate = function
      | CSet (k,vp) -> _read_value vp >>= fun v -> L.return (Set (k,v))
      | CDelete k   -> L.return (Delete k)
    in
    let _2actions cas = 
      let rec loop acc = function
        | [] -> L.return (List.rev acc)
        | ca :: cas -> 
          _translate ca >>= fun a -> 
          loop (a::acc) cas
      in
      loop [] cas
    in
    let _read_commit pos =
      L.bind 
        (L.read log pos)
        (function 
          | Commit c -> L.return c
          | e -> failwith (Printf.sprintf "Catchup:%s is not commit" (entry2s e))
        )
    in
    let rec go_back acc p =
      L.bind 
        (L.read log p) 
        (function
          | Commit c -> 
            let t0 = Commit.get_time c in
            if t0 =>: start 
            then
              let p' = Commit.get_previous c in
              go_back (p::acc) p'
            else
              L.return acc
          | NIL -> L.return acc
          | e -> failwith (Printf.sprintf "Catchup:%s is not commit" (entry2s e))
        )
    in
    go_back [] (L.last log) >>= fun ps -> 
    match ps with
      | [] -> L.return a0
      | ph :: r -> 
        let rec loop acc prev_c = function
          | [] -> L.return acc
          | cp :: tail -> 
            _read_commit cp >>= fun cur_c ->
            let cur_t = Commit.get_time cur_c in
            let prev_t = Commit.get_time prev_c in
            if cur_t =>: prev_t 
            then
              let prev_cas = Commit.get_cactions prev_c in
              _2actions prev_cas >>= fun prev_as ->
              f acc prev_as >>= fun acc' -> loop acc' cur_c tail
            else
              loop acc cur_c tail                    
        in
        _read_commit ph >>= fun prev_c -> loop a0 prev_c r
end
