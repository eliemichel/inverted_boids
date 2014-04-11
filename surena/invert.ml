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

(*
	Résolution dans le cas où les paramètres sont uniformes.
	On suppose alpha et les longueurs constants.
	cm, am ne servent à rien, rm ne sert à rien pour d'autres raisons,
	les paramètres sont :
		cb
		rb
		ab
		ib
	tous de type float.
*)

let alpha = 0.5

let lc = 100.

let lr = 20.

let get_coeff t =
	t.(0), t.(1), t.(2), t.(3)
	(* sale *)

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

let (+++) t1 t2 = Array.init (Array.length t1) (fun i -> t1.(i) +. t2.(i))

let ( *** ) t1 c = Array.init (Array.length t1) (fun i -> t1.(i) *. c)

*)

let sum41 n f =
	let rec aux = function
		| 0 -> ((0.,0.,0.,0.),0.)
		| n ->
			let i = n - 1 in
			let (a,b,c,d),x = f i in
			let (e,f,g,h),y = aux i in
			((a +. e, b +. f, c +. g, d +. h), x +. y) in
	aux n

let calc_grad_single data (cb,rb,ab,ib) i =
	let boids = data.(i) in
	let grad j =
		let sum_c = 
			let v,c = sum2 (Array.length boids) (fun k ->
				let c = sigmoid alpha lc (d boids.(j).pos boids.(k).pos) in
				(boids.(k).pos --- boids.(j).pos) ** c, c) in
			v // c in
		let sum_r =
			sum (Array.length boids) (fun k ->
				not_normalize (boids.(j).pos --- boids.(k).pos)
					** (sigmoid alpha lr (d boids.(j).pos boids.(k).pos))) in
		let sum_a =
			let v,c = sum2 (Array.length boids) (fun k ->
				let c = sigmoid alpha lc (d boids.(j).pos boids.(k).pos) in
				boids.(k).v ** c, c) in
			v // c in
		let v = (sum_c ** cb) ++ (sum_r ** rb) ++ (sum_a ** ab)
			++ (boids.(j).v ** ib) in
		let aux s = 2. *. (scalar s (v -- data.(i+1).(j).v)) in
		(aux sum_c /. 100., aux sum_r, aux sum_a, aux boids.(j).v),
			norm2 (v -- data.(i+1).(j).v) in
	sum41 (Array.length boids) grad
			
let calc_grad n data param =
	let t = Array.init n (fun i -> Random.int (Array.length data - 1)) in
	let (a,b,c,d),cost = sum41 n
		(fun i -> calc_grad_single data param t.(i)) in
	let nf  = float n in
(*	Printf.printf "grad = (%f ; %f ; %f ; %f)\n" a b c d; *)
	(a /. nf, b /. nf, c /. nf, d /. nf), cost /. nf
	
let apply_grad eta n data (cb,rb,ab,ib) =
	let (a,b,c,d),cost = calc_grad n data (cb,rb,ab,ib) in
	let aux x y = x -. eta *. y /. (float (Array.length data.(0))) in
	(aux cb a, aux rb b, aux ab c, aux ib d), cost
	
let read_data name =
	let ic = open_in_bin name in
	let nb_cycles = input_value ic in
	let n = input_value ic in
	let res = Array.init nb_cycles (fun i ->
		Array.init n (fun j -> input_value ic)) in
	close_in ic; res

let nb_gens = ref 1000

let nb_grads = ref 100

let eta = ref 0.001

let name = ref ""

let main () =
	let data = read_data !name in
	let init_param = (0.02,5.,1.5,0.4) in
	let print (cb,rb,ab,ib) =
		Printf.printf "cb = %f\nrb = %f\nab = %f\nib = %f\n" cb rb ab ib in
	let rec loop param = function
		| 0 -> param
		| n ->
			let param,cost = apply_grad !eta !nb_grads data param in
			Printf.printf "====================\n";
			Printf.printf "n = %d\n" (!nb_gens - n);
			print param;
			Printf.printf "c = %f\n%!" cost;
(*			Scanf.scanf "%s\n" (fun s -> ()); *)
			loop param (n-1) in
	let param = loop init_param !nb_gens in
	print param


let () = Arg.parse
	["-n", Arg.Set_int nb_gens,
		"number of cycles to run";
	"-ng", Arg.Set_int nb_grads,
		"number of instances to use when computing the gradient";
	"-l", Arg.Set_float eta,
		"learning rate"]
	(fun s -> name := s)
	"Usage : invert data.\n\
	Tries to recover the parameters given to a simulation from the position\n\
	outut. These data are given as first argument.\n\
	The available options are :";
	main ()

