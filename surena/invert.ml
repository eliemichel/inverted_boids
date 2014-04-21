(*
	Invert tente à partir des données de sortie d'une simulation de retrouver
	les paramètres qui l'ont conduit.
*)

open Engine

let _ = Random.init (truncate (Unix.time ()))

let sigmoida alpha lambda x =
	let d = x -. lambda in
	let e = exp (alpha *. d) in
	let e1 = 1. +. e in
	-. (d *. e) /. (e1 *. e1)
	
let sigmoidl alpha lambda x =
	let e = exp (alpha *. (x -. lambda)) in
	let e1 = 1. +. e in
	(alpha *. e) /. (e1 *. e1)
	
let sigmoidx alpha lambda x =
	let e = exp (alpha *. (x -. lambda)) in
	let e1 = 1. +. e in
	-. (alpha *. e) /. (e1 *. e1)
	
let dsigmoid alpha lambda x =
	let d = x -. lambda in
	let e = exp (alpha *. d) in
	let e1 = 1. +. e in
	let f = e /. (e1 *. e1) in
	let fa = alpha *. f in
	(-. d *. f), fa, (-. fa)

let random_float a b = Random.float (b -. a) +. a

(**
	Résolution dans le cas où les paramètres sont uniformes.
	On suppose alpha et les longueurs constants.
	Les paramètres sont :
		cm
		rm
		am
		im
		sm
	tous de type float.
*)

let alpha = 0.5

let lc = 100.

let lr = 20.

let get_coeff5 t =
	t.(0), t.(1), t.(2), t.(3), t.(4)
	(* horriblement sale *)

(*
let rules n alpha t = 
	let (cb,rb,ab,ib) = get_coeff t in
	Array.([
		make n cb, Cohesion
			(make_matrix n n 1., make_matrix n n alpha, make_matrix n n cl);
		make n rb, Repulsion
			(make_matrix n n 1., make_matrix n n alpha, make_matrix n n rl);
		make n ab, Alignment
			(make_matrix n n 1., make_matrix n n alpha, make_matrix n n al);
		make n ib, Inertia
	])
*)

let (+++) t1 t2 = Array.init (Array.length t1) (fun i -> t1.(i) +. t2.(i))

let ( *** ) t1 c = Array.init (Array.length t1) (fun i -> t1.(i) *. c)

let (///) t c = Array.init (Array.length t) (fun i -> t.(i) /. c)

let sumt1 k n f =
	let rec aux = function
		| 0 -> (Array.make k 0.,0.)
		| n ->
			let i = n - 1 in
			let t1,x = f i in
			let t2,y = aux i in
			(t1 +++ t2, x +. y) in
	aux n

let pre_calc data = Array.init (Array.length data - 1) (fun i ->
	let boids = data.(i) in
	Array.init (Array.length boids) (fun j ->
	
		let sum_c = sum (Array.length boids) (fun k ->
			let c = sigmoid alpha lc (d boids.(j).pos boids.(k).pos) in
			(normalize (boids.(k).pos -- boids.(j).pos)) ** c) in
			
		let sum_r =	sum (Array.length boids) (fun k ->
			(not_normalize (boids.(j).pos -- boids.(k).pos))
				** (sigmoid alpha lr (d boids.(j).pos boids.(k).pos))) in
				
		let sum_a = sum (Array.length boids) (fun k ->
			let c = sigmoid alpha lc (d boids.(j).pos boids.(k).pos) in
			(boids.(k).v -- boids.(j).v) ** c) in
			
		sum_c, sum_r, sum_a))
    

let calc_grad_single data pre_calc t i =
	let cm,rm,am,im,sm = get_coeff5 t in
	let boids = data.(i) in
	let grad j =
		let sum_c, sum_r, sum_a = pre_calc.(i).(j) in
		let sv = stay_v boids.(j).pos in
		let v = (sum_c ** cm) ++ (sum_r ** rm) ++ (sum_a ** am)
			++ (boids.(j).v ** im) ++ (sv ** sm) in
		let aux s = 2. *. (scalar s (v -- data.(i+1).(j).v)) in
		Array.map aux [|sum_c (* /. 100. *); sum_r (* *. 100. *); sum_a;
			boids.(j).v; sv|], norm2 (v -- data.(i+1).(j).v) in
	sumt1 5 (Array.length boids) grad
			
let calc_grad k n data pre_calc param =
(*	let t = Array.init n (fun i -> Random.int (Array.length data - 1)) in *)
	let t = Array.init (Array.length data - 1) (fun x -> x) in
	let n = Array.length t in
	let grad,cost = sumt1 k n
		(fun i -> calc_grad_single data pre_calc param t.(i)) in
	let nf  = float (Array.length data - 1) in
	grad *** (1. /. nf), cost /. nf
	
let apply_grad k eta n data pre_calc param =
	let grad,cost = calc_grad k n data pre_calc param in
	(param +++ (grad *** (-. eta /. ((float (Array.length data.(0)))
		*. (float (Array.length data))))), cost)

(**
	Résolution dans le cas où les paramètres sont uniformes,
	et le boid 0 est un prédateur.
	On suppose alpha et les longueurs constants.
	Les paramètres sont :
		cm
		rm
		am
		im
		sm
	tous de type float.
	rm.(i).(0) = 100.
*)

let lrp = 150.

let rmi0 = 100.

let pre_calc2 data = Array.init (Array.length data - 1) (fun i ->
	let boids = data.(i) in
	Array.init (Array.length boids - 1) (fun j ->
	
		let sum_c = sum (Array.length boids - 1) (fun k ->
			let c = sigmoid alpha lc (d boids.(j+1).pos boids.(k+1).pos) in
			(normalize (boids.(k+1).pos -- boids.(j+1).pos)) ** c) in
			
		let sum_r =	sum (Array.length boids - 1) (fun k ->
			(not_normalize (boids.(j+1).pos -- boids.(k+1).pos))
				** (sigmoid alpha lr (d boids.(j+1).pos boids.(k+1).pos))) in
				
		let sum_a = sum (Array.length boids - 1) (fun k ->
			let c = sigmoid alpha lc (d boids.(j+1).pos boids.(k+1).pos) in
			(boids.(k+1).v -- boids.(j+1).v) ** c) in
		
		let vr = (not_normalize (boids.(j+1).pos -- boids.(0).pos))
			** (sigmoid alpha lrp (d boids.(0).pos boids.(j+1).pos)) in
			
		sum_c, sum_r, sum_a, vr))

let calc_grad_single2 data pre_calc t i =
	let cm,rm,am,im,sm = get_coeff5 t in
	let boids = data.(i) in
	let grad j =
		let sum_c, sum_r, sum_a, vr = pre_calc.(i).(j) in
		let sv = stay_v boids.(j+1).pos in
		let v = (sum_c ** cm) ++ (sum_r ** rm) ++ (sum_a ** am)
			++ (boids.(j+1).v ** im) ++ (sv ** sm)
			++ (vr ** rmi0) in
		let aux s = 2. *. (scalar s (v -- data.(i+1).(j+1).v)) in
		Array.map aux [|sum_c (* /. 100. *); sum_r  ** 100.; sum_a;
			boids.(j+1).v; sv|], norm2 (v -- data.(i+1).(j+1).v) in
	sumt1 5 (Array.length boids - 1) grad

let calc_grad2 k n data pre_calc param =
(*	let t = Array.init n (fun i -> Random.int (Array.length data - 1)) in *)
	let t = Array.init (Array.length data - 1) (fun x -> x) in
	let n = Array.length t in
	let grad,cost = sumt1 k n
		(fun i -> calc_grad_single2 data pre_calc param t.(i)) in
	let nf  = float n in
	grad *** (1. /. nf), cost /. nf
	
let apply_grad2 k eta n data pre_calc param =
	let grad,cost = calc_grad2 k n data pre_calc param in
	(param +++ (grad *** (-. eta /. ((float (Array.length data.(0)))
		*. (float (Array.length data))))), cost)

let read_data name =
	let ic = open_in_bin name in
	let nb_cycles = input_value ic in
	let n = input_value ic in
	let res = Array.init nb_cycles (fun i ->
		Array.init n (fun j -> input_value ic)) in
	close_in ic; res

let nb_gens = ref 1000

let nb_grads = ref 100

let eta = ref 1.

let name = ref ""

let interact = ref false

let main () =
	let data = read_data !name in
	let pre_calc = pre_calc2 data in
	let init_param = [|1.;1.;1.;1.;1.|] in
(*	let init_param = [|0.001;10.;0.01;1.;0.25|] in *)
	let print t =
		let (cb,rb,ab,ib,sb) = get_coeff5 t in
		Printf.printf "cm = %f\nrm = %f\nam = %f\nim = %f\nsm = %f\n"
			cb rb ab ib sb in
	let rec loop param = function
		| 0 -> param
		| n ->
			let param,cost = apply_grad2 5
				(!eta /. sqrt (float (!nb_gens - n + 1)))
				!nb_grads data pre_calc param in
			if !interact then (
				Printf.printf "====================\n";
				Printf.printf "n = %d\n" (!nb_gens - n);
				print param;
				Printf.printf "c = %f\n%!" cost;
				Scanf.scanf "%s\n" (fun s -> ())
			);
			loop param (n-1) in
	let param = loop init_param !nb_gens in
	print param


let () = Arg.parse
	["-n", Arg.Set_int nb_gens,
		"number of cycles to run";
	"-ng", Arg.Set_int nb_grads,
		"number of instances to use when computing the gradient";
	"-l", Arg.Set_float eta,
		"learning rate";
	"-i", Arg.Set interact,
		"interactive mode"]
	(fun s -> name := s)
	"Usage : invert data.\n\
	Tries to recover the parameters given to a simulation from the position\n\
	outut. These data are given as first argument.\n\
	The available options are :";
	main ()

