open Sys
open Unix
open Marshal


module type S = sig
  type 'a process
  type 'a in_port
  type 'a out_port

  val new_channel: unit -> 'a in_port * 'a out_port
  val put: 'a -> 'a out_port -> unit process
  val get: 'a in_port -> 'a process

  val doco: unit process list -> unit process

  val return: 'a -> 'a process
  val bind: 'a process -> ('a -> 'b process) -> 'b process

  val run: 'a process -> 'a
end

module Lib (K : S) = struct

  let ( >>= ) x f = K.bind x f

  let delay f x =
    K.bind (K.return ()) (fun () -> K.return (f x))

  let par_map f l =
    let rec build_workers l (ports, workers) =
      match l with
      | [] -> (ports, workers)
      | x :: l ->
          let qi, qo = K.new_channel () in
          build_workers
            l
            (qi :: ports,
             ((delay f x) >>= (fun v -> K.put v qo)) :: workers)
    in
    let ports, workers = build_workers l ([], []) in
    let rec collect l acc qo =
      match l with
      | [] -> K.put acc qo
      | qi :: l -> (K.get qi) >>= (fun v -> collect l (v :: acc) qo)
    in
    let qi, qo = K.new_channel () in
    K.run
      ((K.doco ((collect ports [] qo) :: workers)) >>= (fun _ -> K.get qi))

end


module Th: S = struct
  type 'a process = (unit -> 'a)

  type 'a channel = { q: 'a Queue.t ; m: Mutex.t; }
  type 'a in_port = 'a channel
  type 'a out_port = 'a channel

  let new_channel () =
    let q = { q = Queue.create (); m = Mutex.create (); } in
    q, q

  let put v c () =
    Mutex.lock c.m;
    Queue.push v c.q;
    Mutex.unlock c.m;
    Thread.yield ()

  let rec get c () =
    try
      Mutex.lock c.m;
      let v = Queue.pop c.q in
      Mutex.unlock c.m;
      v
    with Queue.Empty ->
      Mutex.unlock c.m;
      Thread.yield ();
      get c ()

  let doco l () =
    let ths = List.map (fun f -> Thread.create f ()) l in
    List.iter (fun th -> Thread.join th) ths

  let return v = (fun () -> v)

  let bind e e' () =
    let v = e () in
    Thread.yield ();
    e' v ()

  let run e = e ()
end

module Proc: S = struct 
	type canal = {file: Unix.file_descr; l: int list ref}
	type 'a process = (unit -> 'a)
	type 'a in_port = canal
	type 'a out_port = canal
	
	let new_channel () = let (a,b) = pipe () in  ({file = a; l = ref []},{file = b;l = ref []}) 
	let put_proc v fd () = begin let a = Marshal.to_string v [Marshal.No_sharing] in 
	ignore (Unix.write  fd.file a 0 ((String.length a)-1)); Unix.close fd.file;
					fd.l := (String.length a)::!(fd.l) end
	let put v fd = put_proc v fd ;;
	
	
	let get fd () = let buff = String.create (List.hd (!(fd.l))) in 
	ignore (Unix.read fd.file buff 0 ((String.length buff)-1)); Unix.close fd.file;
	fd.l := List.tl (!(fd.l));  
	let a =  Marshal.from_string buff 0 in a ;;
	

		
	
	let rec aux_doco l sortie entree () = match l with 
	| [] -> ignore (Unix.read sortie "" 0 0)
	| h::q -> match Unix.fork () with 
			| 0 -> h (); ignore (Unix.write entree "" 0 0); Unix.close entree; ()
			| _ -> aux_doco q sortie entree ()
	let doco (l : unit process list) = let (sortie, entree) = pipe () in 
			aux_doco l sortie entree
		
	let return a = (fun () -> a)
	
	let bind e e' () = let (sortie, entree) = pipe () in let v = e () in e' v ()	
		(*match Unix.fork() with 
			| 0 -> e' v (); ignore (Unix.write entree "" 0 0); Unix.close entree
			| _ -> ignore (Unix.read sortie "" 0 0);*)
			
	let run e = e ()
	
	
end
