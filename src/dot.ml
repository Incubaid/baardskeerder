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

open Log
open Entry
module Dot = functor (L:LOG) -> struct

  let dot_tree ?(f= stdout) log =
    Printf.fprintf f "digraph Tree{\n";
    let rec walk pos = 
      match L.read log pos with
	| NIL -> ()
	| Value v -> Printf.fprintf f "\tnode%i [shape = box label = %S];\n" pos v
	| Leaf kps -> 
	  List.iter (fun (_,p) -> walk p) kps;
	  Printf.fprintf f "\tnode%i [shape = record label = \"" pos;
	  let rec loop = function
	    | [] -> ()
	    | [k,p] -> Printf.fprintf f "<%i> %s" p k
	    | (k,p):: tail -> Printf.fprintf f "<%i> %s | " p k; loop tail
	  in
	  loop kps;
	  Printf.fprintf f "\"]\n";
	  List.iter 
	    (fun (k,p) -> 
	      Printf.fprintf f "\tnode%i:<%i> -> node%i\n" pos p p) kps
	  	  
	| Index (pp,kps)  ->
	  walk pp;
	  List.iter (fun (_, p) -> walk p) kps;
	  Printf.fprintf f "\nnode%i [shape = record label=\"" pos;
	  let rec loop = function
	    | [] -> ()
	    | [k,p] -> Printf.fprintf f "<%i> %s" p k
	    | (k,p):: tail -> Printf.fprintf f "<%i> %s | " p k; loop tail
	  in
	  loop kps;
	  Printf.fprintf f "\"]\n";
	  Printf.fprintf f "\tnode%i -> node%i\n" pos pp;
	  List.iter (fun (_, p) -> Printf.fprintf f "\tnode%i -> node%i;\n" pos p) kps
    in
    walk (L.root log);
    Printf.fprintf f "}\n"


  let dot_log ?(f= stdout) log = 
    Printf.fprintf f "digraph Log{\n";
    Printf.fprintf f "\trankdir=\"RL\";\n";
    Printf.fprintf f "\tnode [shape= record];\n";
    let too_far = L.next log in
    let rec loop i = 
      if i = too_far then () 
      else 
	let e = L.read log i in
	let () = match e with
	  | NIL      -> ()
	  | Value v  -> Printf.fprintf f "\tnode%i [label = \"{%i | %s}\"];\n" i i v;
	  | Leaf kps -> 
	    begin
	      Printf.fprintf f "\tnode%i [label = \"{ %i | {" i i;
	      let rec loop = function
		| []          -> ()
		| [k,p]       -> Printf.fprintf f "<%i> %s" p k
		| (k,p)::tail -> Printf.fprintf f "<%i> %s | " p k ; loop tail
	      in
	      loop kps;
	      Printf.fprintf f "}}\"];\n";
	      List.iter 
		(fun (_,p) ->
		  Printf.fprintf f "\tnode%i:<%i> -> node%i;\n" i p p
		) kps
	    end
	  | Index (pp,kps) -> 
	    Printf.fprintf f "\tnode%i [label = \"{%i | { " i i;
	    let rec loop = function
	      | []    -> ()
	      | [k,p] -> Printf.fprintf f "<%i> %s" p k
	      | (k,p) :: tail -> Printf.fprintf f "<%i> %s | " p k; loop tail
	    in
	    loop kps;
	    Printf.fprintf f "}}\"];\n";
	    Printf.fprintf f "\tnode%i -> node%i;\n" i pp;
	    List.iter 
	      (fun (_, p) -> 
		Printf.fprintf f "\tnode%i -> node%i;\n" i p) kps
	in
	let () = 
	  if e <> NIL && i > 0 
	  then Printf.fprintf f "\tnode%i -> node%i [style = invis];\n" i (i-1)
	in
	loop (i+1)
    in 
    loop 0;
    Printf.fprintf f "}\n"
    
  let view ?(v=dot_tree) log = 
    let root = "test" in
    let dot = Filename.temp_file root ".dot" in
    let png = Filename.temp_file root ".png" in
    let oc = open_out dot in
    let () = v ~f:oc log in
    close_out oc;
    let convert_cmd = Printf.sprintf "dot -Tpng -o %s %s" png dot in
    let _ = Sys.command convert_cmd in
    let cmd = Printf.sprintf "evince %s" png in
    Sys.command cmd


  let view_tree log = view ~v:dot_tree log
  let view_log log = view ~v:dot_log log
end
