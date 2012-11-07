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

type t
type tx
type k = string
type v = string
type action = 
  | Set of k * v
  | Delete of k 

type 'a result = | OK of 'a | NOK of k

val init : string -> unit
val make : string -> t
val close : t -> unit

val get_latest : t -> k -> v result
val with_tx : t -> (tx -> 'a result) -> 'a result

val get   : tx -> k -> v result
val set   : tx -> k -> v -> unit

val delete: tx -> k -> unit result


module Logs :
  sig
    module Flog : functor(S: Bs_internal.STORE) -> Log.LOG with type 'a m = 'a S.m
    module Flog0 : functor(S: Bs_internal.STORE) -> Log.LOG with type 'a m = 'a S.m
  end

module Stores :
  sig
    module Memory : Bs_internal.STORE with type 'a m = 'a
    module Sync : Bs_internal.STORE with type 'a m = 'a
    module Lwt : Bs_internal.STORE with type 'a m = 'a Lwt.t
  end

module Pack :
  sig
    type output
    val make_output: int   -> output
    val bool_to  : output  -> bool  -> unit
    val vint_to  : output  -> int   -> unit
    val vint64_to: output  -> int64 -> unit
    val float_to : output  -> float -> unit
    val string_to: output  -> string -> unit
    val option_to: output  -> (output -> 'a -> unit) -> 'a option -> unit
    val string_option_to: output -> string option -> unit
    val list_to  : output  -> (output -> 'a -> unit) -> 'a list -> unit
    val hashtbl_to : output -> (output -> 'a -> 'b -> unit) -> ('a, 'b) Hashtbl.t -> unit
    
    val size_to  : output  -> int -> unit
    val close_output: output -> string
    
    type input
    val make_input   : string -> int -> input

    val input_bool   : input -> bool
    val input_size   : input -> int
    val input_vint   : input -> int
    val input_vint64 : input -> int64
    val input_float  : input -> float
    val input_string : input -> string
    val input_raw    : input -> int -> string
    val input_option : (input -> 'a) -> input -> 'a option
    val input_string_option: input -> string option
    val input_list   : input -> (input -> 'a) -> 'a list
    val hashtbl_from : input -> (input -> 'a * 'b)  -> ('a, 'b) Hashtbl.t
  end


module Baardskeerder :
  functor (LF: functor(S: Bs_internal.STORE) -> Log.LOG with type 'a m = 'a S.m) ->
  functor (S: Bs_internal.STORE) ->
  sig
    type t
    type tx

    val init : string -> unit S.m
    val make : string -> t S.m
    val close : t -> unit S.m

    val get_latest : t -> k -> v result S.m

    
    val range_latest: t -> k option -> bool -> k option -> bool -> int option -> (k list) S.m
    val range_entries_latest: t -> k option -> bool -> k option -> bool -> int option -> (k * v) list S.m
    val rev_range_entries_latest: t -> k option -> bool -> k option -> bool -> int option -> (k * v) list S.m
    val prefix_keys_latest: t -> string -> int option -> (k list) S.m

    val key_count_latest : t -> int S.m

    val with_tx : t -> (tx -> 'a result S.m) -> 'a result S.m

    val get : tx -> k -> v result S.m
    val set : tx -> k -> v -> unit S.m
    val delete : tx -> k -> unit result S.m
    val range : tx -> k option -> bool -> k option -> bool -> int option -> (k list) S.m
    val range_entries : tx -> k option -> bool -> k option -> bool -> int option -> (k *v) list S.m
    val rev_range_entries : tx -> k option -> bool -> k option -> bool -> int option -> (k *v) list S.m
    val prefix_keys : tx -> string -> int option -> (k list) S.m

    val log_update: t -> ?diff:bool -> (tx -> 'a result S.m) -> 'a result S.m
    val last_update: t -> (int64 * (action list)* bool) option S.m
    val commit_last: t -> unit S.m
    val catchup: t -> int64 -> ('a -> int64 -> action list -> 'a S.m) -> 'a -> 'a S.m

    val set_metadata: t -> string -> unit S.m
    val get_metadata: t -> string option S.m
    val unset_metadata: t -> unit S.m
  end
