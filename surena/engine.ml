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

type rule_param = (float array array * float array array * float array array)
type _rule =
	| Cohesion of rule_param
	| Alignment of rule_param
	| Repulsion of rule_param
	| Inertia
type rule = float array * _rule

let real_mod a b =
	if a < 0. then (mod_float a b) +. b
	else mod_float a b 

let real_mod2 (a,b) (c,d) = real_mod a c, real_mod b d

let zero = (0.,0.)

let norm2 (x,y) = x *. x +. y *. y

let norm v = sqrt (norm2 v)

let (++) (a,b) (c,d) = ((a+.c),(b+.d))
let (--) (a,b) (c,d) = ((a-.c),(b-.d))
let ( // ) (a,b) n = ((a/.n),(b/.n))
let ( ** ) (a,b) n = ((a*.n),(b*.n))

let capxi = 800
let capyi = 700
let capx = float capxi
let capy = float capyi
let capv = capx, capy

let (---) (a,b) (c,d) =
	let dx = a -. c in
	let dy = b -. d in
	let rx =
		if dx > capx /. 2. then dx -. capx
		else if dx < -. capx /. 2. then dx +. capx
		else dx in
	let ry =
		if dy > capy /. 2. then dy -. capy
		else if dy < -. capy /. 2. then dy +. capy
		else dy in
	rx,ry

let d (a,b) (c,d) =
	let absx = abs_float (a -. c) in
	let absy = abs_float (b -. d) in
	let dx = if absx < capx /. 2. then absx else capx -. absx in
	let dy = if absy < capy /. 2. then absy else capy -. absy in
	sqrt (dx *. dx +. dy *. dy)

let not_normalize v =
	if v = zero then zero
	else v // (norm2 v)

let random_pos xmin xmax ymin ymax =
	(xmin +. (Random.float (xmax -. xmin))),
	(ymin +. (Random.float (ymax -. ymin)))

let default_boid () = {
	pos = random_pos 0. capx 0. capy;
	v = random_pos (-20.) 20. (-20.) 20.;
	alive = true;
	color = Graphics.red
}

let sigmoid alpha lambda x =
	1. /. (1. +. exp (alpha *. (x -. lambda)))

let coef boids (m, alpha, lambda) i j =
	(sigmoid
		alpha.(i).(j)
		lambda.(i).(j)
		(d boids.(j).pos boids.(i).pos)
	) *. m.(i).(j)

let sum n f =
	let rec aux = function
		| 0 -> zero
		| i -> let i' = i - 1 in f i' ++ aux i'
	in aux n

let sum2 n f =
	let rec aux = function
		| 0 -> zero,0.
		| i -> let i2 = i - 1 in
			let a,b = aux i2 in
			let a2,b2 = f i2 in
			a2 ++ a, b2 +. b
	in aux n

let step_rule_single boids (beta,rule) i =
	let f,s = match rule with
		| Cohesion param ->
			sum2 (Array.length boids) (fun j ->
				let c = coef boids param i j in
				  (boids.(j).pos --- boids.(i).pos) ** c, c
				  	
			)
		| Alignment param ->
			sum2 (Array.length boids) (fun j ->
				let c = coef boids param i j in
					boids.(j).v ** c, c *. (norm boids.(j).v)
			)
		| Repulsion param ->
			sum (Array.length boids) (fun j ->
				let c = coef boids param i j in
					not_normalize (boids.(i).pos --- boids.(j).pos) ** c
			), 1.
		| Inertia -> boids.(i).v, 1.
	in f ** (beta.(i) /. s)

let step_rule boids rule =
	Array.init (Array.length boids) (step_rule_single boids rule)

let add_to_boids boids vec =
	for i = 0 to Array.length boids - 1 do
		boids.(i).v <- boids.(i).v ++ vec.(i) (* *. delta <-- coef pour que ce soit
			homogène, mais osef *)
	done

let update_pos boids =
	Array.iter (fun b ->
		if b.alive
		then b.pos <- real_mod2 (b.pos ++ b.v) capv
	) boids

let step boids rules =
	(**
		`step rules boids`
		met à jour le vecteur de boids à partir des règles données.
	*)
	let applied_rules = List.map (step_rule boids) rules in
		Array.iter (fun boid -> boid.v <- zero) boids;
		List.iter (add_to_boids boids) applied_rules;
		update_pos boids



