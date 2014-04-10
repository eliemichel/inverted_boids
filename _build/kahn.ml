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
	type 'a process = (unit -> 'a)
	type 'a in_port = in_channel
	type 'a out_port = out_channel
	
	let new_channel () = 
	let (a,b) = pipe () in  
	let c  = in_channel_of_descr a in 
	let d = out_channel_of_descr b in 
	(c,d)
	
	
	let put_proc v fd () = 
	Marshal.to_channel fd v  [No_sharing];;
	
	let put v fd = put_proc v fd ;;
	
	
	let get_aux fd () =  
	let a = Marshal.from_channel fd in a;;
	
	
	
	let get fd = get_aux fd
	

		
	
	let rec aux_doco l () = match l with 
	| [] -> ignore(exit 0); 
	| h::q -> match Unix.fork () with 
			| 0 -> aux_doco q  ()
			| _ -> h ();  ignore(Unix.wait ()); ignore (exit 0)
			
	let doco (l : unit process list) = 
	aux_doco l 
		
	let return a = (fun () -> a)
	
	
	let bind_aux e e' ()  =  let v = e () in e' v ()
	let bind e e' = bind_aux e e' 
	
	
			
	let run e = e ()
	
	
end
