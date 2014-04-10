(*
	Invert tente à partir des données de sortie d'une simulation de retrouver
	les paramètres qui l'ont conduit.
*)

open Engine

let _ = Random.init (Unix.time ())

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
	rm ne sert à rien, les paramètres sont :
		cb cm cl
		rb rl
		ab am al
		ib
	tous de type float.
*)



let main filename =
	() (* TODO *)


let () = Arg.parse
	[]
	main
	"Usage : invert data.\n\
	Tries to recover the parameters given to a simulation from the position\n\
	outut. These data are given as first argument.\n\
	The available options are :"


