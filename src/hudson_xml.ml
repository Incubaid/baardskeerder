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

open OUnit

type seq =
  | Lit of string
  | App of (seq * seq)
  | Tag of string * (string * string) list * seq
  | Eol

let eol = Eol
let tab = Lit "  "
let lit s = Lit s
let (++) s0 s1 = App (s0,s1)


let xml_escape s =
  let b = Buffer.create (String.length s) in
  let add_s s = Buffer.add_string b s in
  String.iter
    (function
      | '"'  -> add_s "&quot;"
     | '\'' -> add_s "&apos;"
     | '<'  -> add_s "&lt;"
     | '>'  -> add_s "&gt;"
     | '&'  -> add_s "&amp;"
     | c -> Buffer.add_char b c) s;
  Buffer.contents b


let tag t attrs body = Tag (t,attrs,body)
let seq2s s =
  let b0 = Buffer.create 1024 in
  let add s = Buffer.add_string b0 s in
  let rec walk = function
    | Lit s       -> add s
    | App (s0,s1)   -> walk s0; walk s1
    | Tag (t,a,b) -> add_tag t a b
    | Eol -> add "\n"
  and add_tag t attr body =
    add (Printf.sprintf "<%s " t);
    List.iter (fun (k,v) ->
      let esc = xml_escape v in
      add (Printf.sprintf "%s=%S " k esc)) attr;
    add (Printf.sprintf ">");
    walk body;
    add (Printf.sprintf "</%s>" t)
  in
  walk s;
  Buffer.contents b0


let process result =

  let cn p  = string_of_node (List.hd (List.rev p)) in
  let tn p = string_of_path (List.rev (List.tl(List.tl (List.rev p)))) in
  let testcase p st b =
    tag "testcase" ["classname", cn p;
                    "name", tn p;
                    "time", "0";
                    "status", st;
                   ] b
  in
  let do_one = function
    | RSuccess p     -> testcase p "run" (lit "success")
    | RFailure (p,m) -> testcase p "run" (tag "failure" ["message", m] (lit ""))
    | RSkip (p,_)    -> testcase p "skip" (lit "skip")
    | RTodo (p,_)    -> testcase p "todo" (lit "todo")
    | RError (p,m)   -> testcase p "run" (tag "error" ["message", m] (lit ""))
  in
  let s = lit "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" ++ eol ++
      lit "<testsuite name=\"nosetests\" tests=\"1\" errors=\"1\" failures=\"0\" skip=\"0\">" ++ eol ++
      (List.fold_left (fun acc tr -> acc ++ tab ++ (do_one tr) ++ eol) (lit "") result)
    ++ lit "</testsuite>"
  in
  let chout = open_out "hudson.xml" in
  Pervasives.output_string chout(seq2s s);
  close_out chout


let run_test suite =
  let cb _ = () in
  let result = OUnit.perform_test cb suite in
  process result
