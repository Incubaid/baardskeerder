external _set32_prim : string -> int -> int32 -> unit = "%caml_string_set32"
external _get32_prim : string -> int -> int32 = "%caml_string_get32"

let size_from s pos = _get32_prim s pos |> Int32.to_int
let set_size (b:Bytes.t) size = Int32.of_int size |>_set32_prim b 0

module Pack = struct

  type output = Buffer.t

  let make_output h =
    let h = Buffer.create (h+4) in
    (* size will be filled in here at closing time *)
    Buffer.add_string h "\x00\x00\x00\x00";
    h

  let close_output b =
    let s = Buffer.contents b in
    let size = String.length s - 4 in
    (* set size in the first 4 bytes *)
    set_size s size;
    s

  let size_to b (i:int) =
    let char_at pos =
      let mask = 0xff lsl pos in
      let code = (i land mask) lsr pos in
      Char.unsafe_chr code
    in
    let add pos = Buffer.add_char b (char_at pos) in
    add 0;
    add 8;
    add 16;
    add 24


  let bool_to b (v:bool) =
    let c = if v then '1' else '0' in
    Buffer.add_char b c

  let vint_to b n =
    let rec loop = function
      (* | 0 -> add '\x00' (* unneeded *) *)
      | n when n < 128 -> Buffer.add_char b (Char.unsafe_chr n)
      | n -> let byte = (n land 0x7f) lor 0x80  in
             let () = Buffer.add_char b (Char.unsafe_chr byte) in
             let r = n lsr 7 in
             loop r
    in loop n

  let vint64_to b (n:int64) =
    let max64 = Int64.of_int Pervasives.max_int in
    if n < max64
    then vint_to b (Int64.to_int n)
    else
      let add c = Buffer.add_char b c in
      let rec loop = function
        | n when n < 128L -> add (Char.unsafe_chr (Int64.to_int n))
        | n ->
            let last = (Int64.to_int n) land 0x7f in
            let byte = last lor 0x80  in
            let () = add (Char.chr byte) in
            let r = Int64.shift_right n 7 in
            loop r
      in loop n

  let float_to b (f:float) =
    let (<::) = Int64.shift_left in
    let (>::) = Int64.shift_right in
    let i64 = Int64.bits_of_float f in
    let char_at n =
      let pos = n * 8 in
      let mask = Int64.of_int 0xff <:: pos in
      let code = (Int64.logand i64 mask) >:: pos in
      Char.chr (Int64.to_int code)
    in
    let set x = Buffer.add_char b (char_at x) in
    set 0; set 1; set 2; set 3;
    set 4; set 5; set 6; set 7;
    ()

  let string_to b s =
    let l = String.length s in
    vint_to b l;
    Buffer.add_string b s

  let option_to b a_to = function
    | None   -> bool_to b false
    | Some a -> let () = bool_to b true in
                a_to b a


  let string_option_to b so = option_to b string_to so

  let list_to b e_to list =
    let l = Leaf.length list in
    vint_to b l;
    List.iter (e_to b) list

  let hashtbl_to b e_to ht =
    let l = Hashtbl.length ht in
    vint_to b l;
    Hashtbl.iter (e_to b) ht

  type input = {s:string; mutable p:int}

  let input2s input = Printf.sprintf "{%S;%i}" input.s input.p

  let make_input s p = {s;p}

  let input_char input =
    let c = input.s.[input.p] in
    let () = input.p <- input.p + 1 in
    c

  let input_bool input =
    let c = input_char input in
    match c with
      | '0' -> false
      | '1' -> true
      | _ -> let msg = Printf.sprintf "'%C' is not a bool" c in failwith msg


  let input_size input =
    let p = input.p in
    let () = input.p <- p + 4 in
    size_from input.s p

  let input_vint input =
    let s = input.s in
    let start = input.p in
    let rec loop v shift p =
      let c = s.[p] in
      let cv = Char.code c in
      if cv < 0x80
      then
        let () = input.p <- p+ 1  in
        v + (cv lsl shift)
      else
        let v' = v + ((cv land 0x7f) lsl shift) in
        loop v' (shift + 7) (p+1)
    in loop 0 0 start

  let input_vint64 input =
    let (+:) = Int64.add in
    let ( <<: ) = Int64.shift_left in
    let s = input.s in
    let start = input.p in
    let rec loop v shift p =
      let c = s.[p] in
      let cv_int = Char.code c in
      if cv_int < 0x80
      then
        let () = input.p <- p+ 1  in
        v +: (Int64.of_int cv_int <<: shift)
      else
        let v' = v +: ((Int64.of_int (cv_int land 0x7f)) <<: shift) in
        loop v' (shift + 7) (p+1)
    in loop 0L 0 start

  let input_float input =
    let (<::) = Int64.shift_left
    and (|::) = Int64.logor
    in
    let p = input.p in
    let i64 i= Int64.of_int (Char.code input.s.[p + i]) in
    let b0 = i64 0
    and b1s = i64 1 <:: 8
    and b2s = i64 2 <:: 16
    and b3s = i64 3 <:: 24
    and b4s = i64 4 <:: 32
    and b5s = i64 5 <:: 40
    and b6s = i64 6 <:: 48
    and b7s = i64 7 <:: 56 in
    let r =
      b0 |:: b1s |:: b2s |:: b3s
      |:: b4s |:: b5s |:: b6s |:: b7s
    in
    let () = input.p <- input.p + 8 in
    Int64.float_of_bits r


  let input_string input =
    let l = input_vint input in
    let s = String.sub input.s input.p l in
    let () = input.p <- input.p + l in
    s

  let input_raw input l =
    let s = String.sub input.s input.p l in
    let () = input.p <- input.p + l in
    s

  let input_option input_a input =
    let some = input_bool input in
    if some
    then let a = input_a input in Some a
    else None

  let input_string_option input = input_option input_string input

  let input_list input input_e =
    let l = input_vint input in
    let rec loop acc = function
      | 0 -> List.rev acc
      | n -> let e = input_e input in
             loop (e :: acc) (n-1)
    in
    loop [] l

  let hashtbl_from input e_from =
    let l = input_vint input in
    let ht = Hashtbl.create l in
    let rec loop = function
      | 0 -> ht
      | i ->
          let k, v = e_from input in
          Hashtbl.replace ht k v;
          loop (i-1)
    in loop l

end
