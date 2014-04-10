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
	On suppose alpha constant.
	cm, am ne servent à rien, rm ne sert à rien pour d'autres raisons,
	les paramètres sont :
		cb cl
		rb rl
		ab al
		ib
	tous de type float.
*)

let alpha = 0.5

let get_coeff t =
	t.(0), t.(1), t.(2), t.(3), t.(4), t.(5), t.(6)
	(* sale *)

let rules n alpha t = 
	let (cb,cl,rb,rl,ab,al,ib) = get_coeff t in
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

let calc_grad_single data rules i =
	let boids = step data.(i) rules in
	()

let read_data name =
	let ic = open_in_bin name in
	let nb_cycles = input_value ic in
	let n = input_value ic in
	Array.init nb_cycles (fun i ->
		Array.init n (fun j -> input_value ic))

let main filename =
	() (* TODO *)


let () = Arg.parse
	[]
	main
	"Usage : invert data.\n\
	Tries to recover the parameters given to a simulation from the position\n\
	outut. These data are given as first argument.\n\
	The available options are :"


