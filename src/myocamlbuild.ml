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

open Ocamlbuild_pack
open Ocamlbuild_plugin
open Command

let _ = dispatch & function
  | After_rules ->
      flag ["ocaml"; "byte"; "link"] (S[A"-custom"]);

      flag ["compile"; "c";]
        (S[
          A"-ccopt"; A"-Wall";
          A"-ccopt"; A"-Wextra";
          A"-ccopt"; A"-Werror";
          A"-ccopt"; A"-O3";
        ]);
      flag ["ocaml"; "use_libbaardskeerder"; "link"; "library"; "byte"] & 
        S[A"-cclib"; A"-lbaardskeerder_c";];

      flag ["c"; "use_libbaardskeerder"; "ocamlmklib"] & 
        S[ A"-lbaardskeerder_c";];

      dep ["link"; "ocaml"; "link_libbaardskeerder"] 
        ["libbaardskeerder_c.a"];
        
      flag ["compile"; "c"]
        (S[A"-ccopt"; A"-I.."; A"-ccopt"; A"-msse4.2"]);

  | _ -> ()
