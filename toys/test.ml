type v = string
type k = string
type pos = int

type entry = 
  | NIL
  | V of v
  | L of k * pos
  | N of pos * k * pos

type log = {es:entry array;  mutable next : int;}
      
let make cap = {es = Array.make cap NIL; next = 0;}
let root_pos log = log.next -1
let get_entry log pos = if pos = -1 then NIL else log.es.(pos)
let get_length log = Array.length log.es
let free log = get_length log - log.next
  
let write log es =
  let do_one e = 
    if free log = 0 then failwith "full";
    let p = log.next in
    log.es.(p) <- e;
    log.next <- p + 1 
  in
  List.iter do_one es

let getk log k c = 
  let rec find_pos pos = 
    match get_entry log pos with
      | NIL -> failwith "Not_found"
      | V v -> c v
      | L (k0,p0) -> if k = k0 then find_pos p0 else failwith "Not_found"
      | N (l,k0,r) -> let p' = if k <= k0 then l else r  in
		      find_pos p'			
  in
  find_pos (root_pos log)
    
let get log k = getk log k (fun v -> v)

let dump log = 
  Array.iteri (fun i e->
    let () = Printf.printf "%2i: " i in
    match e with
      | NIL        -> print_newline()
      | V v        -> Printf.printf "V \"%s\"\n" v
      | L (k,p)    -> Printf.printf "L(\"%s\",%i)\n" k p 
      | N (l,k0,r) -> Printf.printf "N(%i,\"%s\",%i)\n" l k0 r
  ) log.es

let dot_log ?(f = stdout) log = 
  Printf.fprintf f "digraph Log{\n";
  Printf.fprintf f "\trankdir=\"RL\";\n";
  Printf.fprintf f "\tnode [shape=record];\n";
  let () = Array.iteri (fun i e ->
    let () = match e with
      | NIL -> ()
      | V v     -> 
	Printf.fprintf f "\tnode%i [label = \"{%i|%s}\"];\n" i i v
      | L (k,p) -> 
	Printf.fprintf f 
	  "\tnode%i [label = \"{%i | { %s | <f1> %i} }\"];\n" 
	  i i k p;
	Printf.fprintf f "\tnode%i:<f1> -> node%i;\n" i p
      | N(l,k0,r)  -> Printf.fprintf f "\tnode%i [label = \"{%i| { <f1> %i | %s | <f2> %i}}\"];\n" i i l k0 r;
	Printf.fprintf f "\tnode%i:<f1> -> node%i;\n" i l;
	Printf.fprintf f "\tnode%i:<f2> -> node%i;\n" i r;
    in
    if e <> NIL && i > 0 then Printf.fprintf f "\tnode%i -> node%i [style = invis];\n" i (i-1)
  ) log.es in
  Printf.fprintf f "}"
    

let dot_tree ?(f= stdout) log =
  Printf.fprintf f "digraph Tree{\n";
  let rec walk pos = 
    match get_entry log pos with
      | NIL -> ()
      | V v -> Printf.fprintf f "\tnode%i [label = %S shape = box];\n" pos v
      | L(k,p) -> 
	walk p;
	Printf.fprintf f "\tnode%i [label = %S shape = oval];\n" pos k;
	Printf.fprintf f "\tnode%i -> node%i\n" pos p;
	
      | N(l,k,r)  -> 
	walk l;
	walk r;
	Printf.fprintf f "\tnode%i [label = %S shape = trapezium];\n" pos k;
	Printf.fprintf f "\tnode%i -> node%i\n" pos l;
	Printf.fprintf f "\tnode%i -> node%i\n" pos r
  in
  walk (root_pos log);
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
let view_log  log = view ~v:dot_log log

    
    

type dir = Hit | Left | Right
type trail = (dir * entry * pos) list
exception Todo of trail * string

let todo v msg = raise (Todo (v,msg))


let set log k v =
  let rec build_set (visited:trail)  = V v :: do_start (log.next) k visited 
  and do_start start k visited = match visited with
    | [] -> [L (k, start) ]
    | (Hit ,  L(k0,p0),pe) :: rest -> 
      L(k, start) :: do_rest (start + 1 ) rest
    | (Left,  L(k0,p0),pe) :: rest -> 
      L(k, start) :: N (start + 1, k, pe)  :: do_rest (start + 2) rest
    | (Right, L(k0,p0),pe) :: rest -> 
      L(k, start) :: N (p0 , k0,start + 1) :: do_rest (start + 2) rest
    | _ -> todo visited (Printf.sprintf "do_start %i '%s'" start k)
  and do_rest start visited = 
    match visited with
      | [] -> []
      | (Left, N(pl,k0,pr),pe) :: rest -> 
	N (start, k0, pr) :: do_rest (start + 1) rest
      | (Right, N(pl,k0,pr),pe) :: rest ->
	N (pl,k0,start) :: do_rest (start + 1) rest
      | _ -> todo visited (Printf.sprintf "do_rest %i" start)
  in
  let rec descend visited pos = 
    match get_entry log pos with
      | NIL -> visited
      | V v -> failwith "corrupt"
      | L (k0,p0) as e when k < k0   -> (Left ,e, pos) :: visited
      | L (k0,p0) as e when k = k0   -> (Hit  ,e, pos) :: visited
      | L (k0,p0) as e               -> (Right,e, pos) :: visited
      | N (l,k0,r) as e when k <= k0 -> descend ((Left,e, pos):: visited) l
      | N (l,k0,r) as e              -> descend ((Right,e,pos) :: visited) r
  in
  let visited = descend [] (root_pos log) in
  let path = build_set visited in
  write log path


let t0 = make 30 ;;
List.iter (fun (k,v) -> set t0 k v) 
  ["f","F";  
   "d","D";
   "h","H";
   "a","A";
   "z","Z";
  ];;




