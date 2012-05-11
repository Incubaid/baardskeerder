open Log
open Commit
open Entry
open Time
open Base

module Catchup(L: LOG) = struct


  let (>>=) = L.bind 

  let read_value log pos = 
    (L.read log pos) >>= (function 
      | Value v -> L.return v
      | e -> failwith (Printf.sprintf "Catchup:%s is not a value" (entry2s e))
     )

  let read_commit log pos =
    L.bind 
      (L.read log pos)
      (function 
        | Commit c -> L.return c
        | e -> failwith (Printf.sprintf "Catchup:%s is not commit" (entry2s e))
      )


  let translate_caction log = function
    | CSet (k,vp) -> read_value log vp >>= fun v -> L.return (Set (k,v))
    | CDelete k   -> L.return (Delete k)
 

  let translate_cactions log cas = 
    let rec loop acc = function
      | [] -> L.return (List.rev acc)
      | ca :: cas -> 
        translate_caction log ca >>= fun a -> 
        loop (a::acc) cas
    in
    loop [] cas
 

  let catchup (i0: int64) (f : 'a -> int64 -> action list -> 'a L.m) (a0:'a) (log : L.t) =
    let start = (i0, 0,false) in
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
          | NIL -> 
            L.return acc
          | e -> failwith (Printf.sprintf "Catchup:%s is not commit" (entry2s e))
        )
    in
    go_back [] (L.last log) >>= fun ps -> 
    match ps with
      | [] -> L.return a0
      | ph :: r -> 
        let rec loop acc prev_c = function
          | [] -> 
            if Commit.is_explicit prev_c then 
              let prev_cas = Commit.get_cactions prev_c in
              translate_cactions log prev_cas >>= fun prev_as ->              
              let prev_t = Commit.get_time prev_c in
              let prev_i = Time.major_of prev_t in
              f acc prev_i prev_as >>= fun acc' ->
              L.return acc'
            else L.return acc
          | cp :: tail -> 
            read_commit log cp >>= fun cur_c ->
            let cur_t = Commit.get_time cur_c in
            let prev_t = Commit.get_time prev_c in
            if cur_t >: prev_t 
            then
              let prev_cas = Commit.get_cactions prev_c in
              translate_cactions log prev_cas >>= fun prev_as ->
              let prev_i = Time.major_of prev_t in
              f acc prev_i prev_as >>= fun acc' -> loop acc' cur_c tail
            else
              loop acc cur_c tail                    
        in
        read_commit log ph >>= fun prev_c -> loop a0 prev_c r
end
