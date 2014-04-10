open Graphics
open Printf

type rule = boid array -> int -> (float * float)
and boid = { pos : float * float; v : float * float;
	behavior : (rule * float) list;
	color : color }

let zero = (0.,0.)

let norm (x,y) = x *. x +. y *. y

let (++) (a,b) (c,d) = ((a+.c),(b+.d))
let (--) (a,b) (c,d) = ((a-.c),(b-.d))
let ( // ) (a,b) n = ((a/.n),(b/.n))
let ( ** ) (a,b) n = ((a*.n),(b*.n))

let d a b = norm (a -- b)
	
let stay xmin xmax ymin ymax boids boid =
	let x,y = boids.(boid).pos in
	let a = 1. in
	let ax = if x < xmin then a
		else if x > xmax then -. a
		else 0. in
	let ay = if y < ymin then a
		else if y > ymax then -. a
		else 0. in
	ax,ay

let herd epsilon boids boid =
	let sum = ref zero in
	let nb = ref 0 in
	for i = 0 to Array.length boids - 1 do
		if i <> boid && d boids.(i).pos boids.(boid).pos < epsilon then (
			sum := !sum ++ boids.(i).pos;
			incr nb
		)
	done;
	if !nb > 0 then ((!sum // (float !nb)) -- boids.(boid).pos)
	else zero
	
let keep epsilon boids i =
	let sum = ref zero in
	for j =0 to Array.length boids - 1 do
		let d = d boids.(i).pos boids.(j).pos in
		if i <> j && d < epsilon then
			sum := !sum ++ ((boids.(i).pos -- boids.(j).pos) // (d))
	done;
	!sum
	
let speed epsilon boids i =
	let sum = ref zero in
	let nb = ref 0 in
	for j = 0 to Array.length boids - 1 do
		if i <> j && d boids.(i).pos boids.(j).pos < epsilon then (
			sum := !sum ++ boids.(j).v;
			incr nb
		)
	done;
	if !nb > 0 then (!sum  // (float !nb)   )
	else zero

let follow j epsilon boids i =
	let d = d boids.(j).pos boids.(i).pos in
	if d > epsilon then
	  	(boids.(j).pos -- boids.(i).pos) // (sqrt d)
	else zero

let goal pos epsilon boids i =
	let d = d boids.(i).pos pos in
	if d > epsilon then pos -- boids.(i).pos
	else zero
	
let brown boids i =
	Random.float 2. -. 1., Random.float 2. -. 1.
	
let avoid j epsilon boids i =
	let d = d boids.(j).pos boids.(i).pos in
	if d < epsilon then boids.(i).pos -- boids.(j).pos
	else zero

let rand p boids i =
	if Random.float 1. < p then
		Random.float 2. -. 1., Random.float 2. -. 1.
	else zero
	
let move threshold boids i =
	if norm boids.(i).v < threshold then
		boids.(i).v
	else zero

let cap = 999999999.

let apply_rules boids i =
	let sum = List.fold_left (fun (sum) (rule,f) ->
		let a = rule boids i in
		if norm a > 0. then (sum ++ (a ** f))
		else (sum)) (zero) boids.(i).behavior in
	let a = sum in
	let v =
		let v = (boids.(i).v ** 0.95) ++ a in
		(* let v = a in *)
		if norm v > cap then v ** (sqrt (cap /. (norm v)))
		else v in
	{ boids.(i) with v = v; pos = boids.(i).pos ++ v }	

let rules_boids = [
	stay 50. 550. 50. 550.,
		1.;
	herd 5000.,
		0.025;
	keep 1000.,
		10.;
	speed 5000.,
		0.1
]

let wanderer = [
	stay 50. 550. 50. 550.,
		0.5;
	keep 400.,
		2.;
	speed 5000.,
		0.01;
	rand 0.01,
		10.;
	move 5.,
		3.
]

let brownian = [
	stay 50. 550. 50. 550.,
		1.;
	keep 1000.,
		2.;
	speed 5000.,
		0.05;
	brown,
		10.
]

let follower j = [
	stay 50. 550. 50. 550.,
		0.5;
	keep 400.,
		2.;
	speed 5000.,
		0.01;
	follow j 1000.,
		0.1
]

let predator = [
	stay 50. 550. 50. 550.,
		0.5;
	herd 5000.,
		5.;
	rand 0.01,
		10.
]

let simple = [
	stay 50. 550. 50. 550.,
		0.5;
	speed 5000.,
		0.1
]

let random_pos xmin xmax ymin ymax =
	(xmin +. (Random.float (xmax -. xmin))),
	(ymin +. (Random.float (ymax -. ymin)))

(*let minisleep sec =
    ignore (Unix.select [] [] [] sec) *)

let n = 200

let n2 = 16

let nb_etapes = 5000

let () =
	Random.self_init ();
	open_graph " 600x600";
	set_window_title "Boids";
	auto_synchronize false;
	(* let boids = ref (Array.init n (fun i ->
		{ pos = random_pos 0. 600. 0. 600.; v = zero; behavior = rules_boids; color = red })) in *)
	(* let boids = ref (Array.init n2 (function
		| n when n < n2 - 1 -> { pos = random_pos 0. 600. 0. 600.; v = zero; behavior = wanderer; color = if n = 0 then green else red }
		| 15 -> { pos = random_pos 0. 600. 0. 600.; v = zero; behavior = follower 0; color = blue }
		| _ -> assert false)) in *)
	(* let boids = ref (Array.init n (function
		| 0 -> { pos = random_pos 0. 600. 0. 600.; v = zero; behavior = predator; color = red }
		| n -> { pos = random_pos 0. 600. 0. 600.; v = zero; behavior = (avoid 0 5000., 0.05)::rules_boids; color = green })) in *)
	let boids = ref (Array.init n (fun i ->
		{ pos = random_pos 0. 600. 0. 600.; v = random_pos (-10.) 10. (-10.) 10.; behavior = simple; color = black })) in
	(* let i = ref 0 in
	let mat = Array.make_matrix n2 n2 0. in *)
	while not (key_pressed () && read_key () = 'q') do
		clear_graph ();
		Array.iter (fun boid -> let x,y = boid.pos in
			set_color boid.color; fill_circle (truncate x) (truncate y) 3) !boids;
		(* for i = 0 to n2 - 2 do
			for j = i + 1 to n2 - 1 do
				mat.(i).(j) <- mat.(i).(j) +. (sqrt (d !boids.(i).pos !boids.(j).pos))
			done
		done;
		incr i;
		printf "%d\n" !i;
		if !i = nb_etapes then (
			let oc = open_out "plopiplop" in
			for i = 0 to n2 - 2 do
				for j = i + 1 to n2 - 1 do
					fprintf oc "(%d,%d) :\t\t%f\n" i j (mat.(i).(j) /. (float nb_etapes))
				done
			done;
			close_out oc;
		); *)
		synchronize ();
		let boids2 = Array.init (Array.length !boids) (fun i -> apply_rules !boids i) in
		boids := boids2;
		ignore (Unix.system "sleep 0.02");
	done;
	close_graph ()
