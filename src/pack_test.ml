open OUnit

open Pack
let k = 1000 
let m = k * k 

let sample = [| 
  1;2;3;4;5;6;7;8;9;
  12;13;15;42;
  123;124;
  160;200;255;256;257;300;
  1000;1024;1234;1500;
  4096;4100;5200;
  12345;16 * k;
  123456;1234567;2345678;
  65537;m;
  40 * m; 60 * m; 100 * m;
  1000 * m;
             |]
 
let measure f = 
  let a0 = Gc.allocated_bytes () in
  let t0 = Unix.gettimeofday () in
  let () = f () in
  let t1 = Unix.gettimeofday () in
  let a1 = Gc.allocated_bytes () in
  let dt = t1 -. t0 in
  let da = a1 -. a0 in
  dt,da

let vint_to_performance () = 
  let rec loop i = 
    if i = 0 then ()
    else
      let b = Pack.make_output 64 in
      let () = Array.iter (fun v ->Pack.vint_to b v) sample in
      let bs = Pack.close_output b in
      assert (String.length bs < 100);
      loop (i-1)
  in
  let n = 10 * 1000 * 1000 in
  let dt,da = measure (fun () -> loop n) in
  let mega_da = da /. (1024.0 *. 1024.0) in
  Printf.printf "\nn=%i;dt=%fs;da=%fMB\n" n dt mega_da

let size_performance() = 
  let s = "xxxxxxxx" in
  let rec loop i = 
    if i = 0 then ()
    else
      let () = Array.iter 
        (fun v ->
          set_size s v;
          let v' = size_from s 0 in
          (* OUnit.assert_equal ~printer:string_of_int v v'; *)
          assert (v' = v);
          ()
        ) sample
      in
      loop (i-1)
  in
  let n = 10 * 1000 * 1000 in
  let dt,da = measure (fun () -> loop n) in
  let mega_da = da /. (1024.0 *. 1024.0) in
  Printf.printf "\nn=%i;dt=%fs;da=%fMB\n" n dt mega_da


let size2_correctness () = 
  let s = "xxxxxxxx" in
  let () = Array.iter 
    (fun v ->
      Cpack.set_size_unsafe s v;
      let v_ocaml = size_from s 0 in
      let () = Printf.printf "%08i:%S\n%!" v_ocaml s in    
      let v_c  = Cpack.size_from_unsafe s 0 in
      OUnit.assert_equal ~printer:string_of_int v_ocaml v_c ~msg:(Printf.sprintf "%S: " s);
      OUnit.assert_equal ~printer:string_of_int v v_c       ~msg:(Printf.sprintf "%S: " s)
    ) sample
  in
  ()

let size2_performance() = 
  let s = "xxxxxxxx" in
  let rec loop i = 
    if i = 0 then ()
    else
      let () = Array.iter
        (fun v ->
          Cpack.set_size_unsafe s v;
          let v' = Cpack.size_from_unsafe s 0 in
          (*OUnit.assert_equal ~printer:string_of_int v v';*)
          assert (v = v');
          ()
        ) sample
      in
      loop (i - 1)
  in
  let n = 10 * 1000 * 1000 in
  let dt,da = measure (fun () -> loop n) in
  let mega_da = da /. (1024.0 *. 1024.0) in
  Printf.printf "\nn=%i;dt=%fs;da=%fMB\n" n dt mega_da


let list_correctness () =
  let xs = [1;2;3;4;5;6] in
  let out = Pack.make_output 128 in
  let () = Pack.list_to out Pack.vint_to xs in
  let buffer = Pack.close_output out in
  let input = Pack.make_input buffer 4 in
  let xs' = Pack.input_list input Pack.input_vint in
  OUnit.assert_equal xs xs'

let suite = 
  "Pack">:::[
    "vint_to_performance" >:: vint_to_performance;
    "size_performance" >:: size_performance;
    "size2_correctness" >:: size2_correctness;
    "size2_performance" >:: size2_performance;
    "list_correctness" >:: list_correctness;
  ]
