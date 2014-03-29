(*
	Engine exécute la simulation du systeme et définit des types et procédures
	permettant sa manipulation.
*)


type boid = {
	mutable pos   : float * float;
	mutable v     : float * float;
	alive : bool; (* true = agent ; false = Décors *)
	color : Graphics.color
}

type rule_param = float array array * float array array * float array array
type rule =
	| Cohesion of rule_param
	| Alignment of rule_param

let zero = (0.,0.)

let norm2 (x,y) = x *. x +. y *. y

let norm v = sqrt (norm2 v)

let (++) (a,b) (c,d) = ((a+.c),(b+.d))
let (--) (a,b) (c,d) = ((a-.c),(b-.d))
let ( // ) (a,b) n = ((a/.n),(b/.n))
let ( ** ) (a,b) n = ((a*.n),(b*.n))

let d a b = norm2 (a -- b)

let random_pos xmin xmax ymin ymax =
	(xmin +. (Random.float (xmax -. xmin))),
	(ymin +. (Random.float (ymax -. ymin)))

let default_boid () = {
	pos = random_pos 0. 600. 0. 600.;
	v = random_pos (-10.) 10. (-10.) 10.;
	alive = true;
	color = Graphics.black
}

let sigmoid alpha lambda x =
	1. /. (1. +. exp (alpha *. (x -. lambda)))

let coef boids (m, alpha, lambda) i j =
	(sigmoid
		alpha.(i).(j)
		lambda.(i).(j)
		(norm (boids.(j).pos -- boids.(i).pos))
	) *. m.(i).(j)

let sum n f =
	let rec aux = function
		| -1 -> zero
		| i -> let i' = i - 1 in f i' ++ aux i'
	in aux n

let step_rule_single boids rule i = match rule with
	| Cohesion param ->
		sum (Array.length boids) (fun j ->
			(boids.(j).pos -- boids.(i).pos) ** (coef boids param i j)
		)
	| Alignment param ->
		sum (Array.length boids) (fun j ->
			boids.(j).v ** (coef boids param i j)
		)

let step_rule boids rule =
	Array.init (Array.length boids) (step_rule_single boids rule)

let add_to_boids boids vec =
	for i = 0 to Array.length boids - 1 do
		boids.(i).v <- boids.(i).v ++ vec.(i) (* *. delta <-- coef pour que ce soit
			homogène, mais osef *)
	done

let update_pos boids =
	Array.iter (fun b -> b.pos <- b.pos ++ b.v) boids

let step boids rules =
	(**
		`step rules boids`
		met à jour le vecteur de boids à partir des règles données.
	*)
	let applied_rules = List.map (step_rule boids) rules in
		List.iter (add_to_boids boids) applied_rules;
		update_pos boids



