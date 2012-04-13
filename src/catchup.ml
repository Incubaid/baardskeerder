open Log
open Commit
open Entry
open Time
open Base

module Catchup(L: LOG) = struct
  type caction = 
    |CSet of k * v
    |CDelete of k

  let catchup (start:Time.t) (f : 'a -> action list -> 'a L.m) (a0:'a) (log : L.t) =
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
    L.bind 
      (go_back [] (L.last log))
      (fun ps -> 
        match ps with
          | [] -> L.return a0
          | ph :: r -> 
            let rec loop acc prev_c = function
              | [] -> L.return acc
              | cp :: tail -> 
                L.bind 
                  (_read_commit cp)
                  (fun cur_c ->
                    let cur_t = Commit.get_time cur_c in
                    let prev_t = Commit.get_time prev_c in
                    if cur_t =>: prev_t 
                    then
                      let prev_as = Commit.get_actions prev_c in
                      L.bind 
                        (f acc prev_as)
                        (fun acc' -> loop acc' cur_c tail)
                    else
                      (loop acc cur_c tail)
                  ) 
            in
            L.bind 
              (_read_commit ph)
              (fun prev_c -> loop a0 prev_c r)
      )
end
