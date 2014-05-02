(*
	Display affiche une simulation, potentiellement personnalisable en donnant
	des arguments.
*)

open Graphics
open Scanf
open Printf

open Engine

let dump_file = ref ""

let _ = Random.init (int_of_float (100. *. Unix.time ()))

let viewport_x = ref 0.
let viewport_y = ref 0.
let viewport_w = float_of_int Engine.capxi
let viewport_h = float_of_int Engine.capyi
let viewport_scale = ref 1.




let draw_boid boid =
	let x,y = boid.pos in
		set_color boid.color;
		fill_circle
			(truncate ((x -. !viewport_x) *. !viewport_scale +. viewport_w /. 2.))
			(truncate ((y -. !viewport_y) *. !viewport_scale +. viewport_h /. 2.))
			3

let n = 200

let uniform n a = Array.init n (fun _ -> Array.make n a)

let quadblock n a b c d =
	Array.init n (fun i -> Array.init n (fun j ->
		if i < n/2
		then
			if j < n/2
			then a
			else b
		else
			if j < n/2
			then c
			else d
	))
	(** Array.make_matrix... *)
(* let rules = [
	Array.make n 0.01, Cohesion (uniform n 1., uniform n 0.5, uniform n 100.);
	Array.make n 15., Repulsion (uniform n 1., uniform n 2., uniform n 20.);
	Array.make n 1., Alignment (uniform n 1., uniform n 0.5, uniform n 100.);
] *)
(* TODO : Procédures pratiques de construction de règle *)

let rules cm ca cl rm ra rl am aa al im sm =
(*	let cb = Array.make n cb in *)
	let cm = uniform n cm in
	let ca = uniform n ca in
	let cl = uniform n cl in
(*	let rb = Array.make n rb in *)
	let rm = uniform n rm in
	let ra = uniform n ra in
	let rl = uniform n rl in
(*	let ab = Array.make n ab in *)
	let am = uniform n am in
	let aa = uniform n aa in
	let al = uniform n al in
	let im = Array.make n im in
	let sm = Array.make n sm in
	let rules = [
		Cohesion (cm,ca,cl);
		Repulsion (rm,ra,rl);
		Alignment (am,aa,al);
		Inertia im;
		Stay sm
	] in
	rules,(cm,ca,cl),(rm,ra,rl),(am,aa,al),im,sm


let rules =
	let rules,(cm,_,cl),(rm,_,rl),(am,_,_),im,sm = rules
		0.001 0.5 100.
		10. 0.5 20.
		0.01 0.5 100.
		1.
		0.25 in
(*	let rules,(cb,_,_,_),(_,rm,ra,rl),_,_ = rules
		0.0073 1. 0.5 100.
		5.002 1. 0.5 20.
		0.73849 1. 0.5 100.
		0.363 in
*)
	im.(0) <- 0.95;
	sm.(0) <- 1.;
	for i = 1 to n - 1 do
		cl.(0).(i) <- 300.;
		cm.(0).(i) <- 0.1;
		cm.(i).(0) <- 0.;
		am.(i).(0) <- 0.;
		am.(0).(i) <- 0.;
		rl.(i).(0) <- 150.;
		rm.(i).(0) <- 100.;
		rm.(0).(i) <- 0.
	done;
	rules


(* 
let rules =
	let cb = Array.make n 0.01 in
	let cm = quadblock n 1. 0. 0. 1. in
	let ca = quadblock n 0.5 0. 0. 0.5 in
	let cl = quadblock n 100. 0. 0. 100. in
	let rb = Array.make n 10. in
	let rm = quadblock n 1. 0. 0. 1. in
	let ra = quadblock n 2. 0. 0. 2. in
	let rl = quadblock n 20. 0. 0. 20. in
	let ab = Array.make n 0.5 in
	let am = quadblock n 1. 0. 0. 1. in
	let aa = quadblock n 0.5 0. 0. 0.5 in
	let al = quadblock n 100. 0. 0. 100. in
	let ib = Array.make n 0.5 in
	let rules = [
		cb, Cohesion (cm,ca,cl);
		rb, Repulsion (rm,ra,rl);
		ab, Alignment (am,aa,al);
		ib, Inertia
	] in
	cb.(0) <- 0.1;
	for i = 1 to n - 1 do
		rl.(i).(0) <- 100.;
		ra.(i).(0) <- 1.;
		rm.(i).(0) <- 10.
	done;
	rules
*)

let nb_cycles = ref (-1)

let boids = Array.init n (fun i -> default_boid ())

let () =
	boids.(0) <- { boids.(0) with color = Graphics.black };
	for k = 1 to n/2 - 1 do
		boids.(k) <- { boids.(k) with color = Graphics.blue }
	done


let stringtail str i =
	String.sub str i ((String.length str) - i)



let simu () =
	let oc =
		if !dump_file = "" then stdout
		else open_out_bin !dump_file in
	if !dump_file = "" then (
		let s = " " ^ (string_of_int Engine.capxi) ^ "x"
			^ (string_of_int Engine.capyi) in
		open_graph s;
		set_window_title "Boids";
		auto_synchronize false
	) else (
		output_value oc !nb_cycles;
		output_value oc n
	);
	let i = ref 0 in
	while !dump_file = "" && not (key_pressed () && read_key () = 'q')
		|| !nb_cycles > 0 && !i < !nb_cycles do
		if !dump_file = "" then (
			clear_graph ();
			Array.iter draw_boid boids;
			synchronize ();
			ignore (Unix.system "sleep 0.02")
		) else Array.iter (output_value oc) boids;
		Engine.step boids rules;
		incr i
	done;
	if !dump_file = "" then 
		close_graph ()
	else
		close_out oc;
	exit(0)

exception Quit
let cli () =
	print_endline "Surena, v0.0.1";
	print_endline "type `help` for more information about that CLI";
	print_endline "";
	try
		while true do
			print_string "Surena ⋅> ";
			try
				let cmd = read_line () in
				try sscanf cmd "help" ();
					print_endline "Surena, v0.0.1";
					print_endline "";
					print_endline "Available commands are :";
					print_endline " - help: display that list";
					print_endline " - quit: close Surena";
					print_endline " - print boid \027[01mn\027[0m: display information about the \027[01mn\027[0mth boid";
					print_endline " - update boid \027[01mn\027[0m set ";
					print_endline "     [position=(\027[01mx\027[0m,\027[01my\027[0m)]";
					print_endline "     [velocity=(\027[01mvx\027[0m,\027[01mvy\027[0m)]";
					print_endline "     [alive=yes|true|no|false]";
					print_endline "     [color=(\027[01mr\027[0m,\027[01mg\027[0m,\027[01mb\027[0m)]";
					print_endline "    Edit boid info";
					print_endline " - update viewport set";
					print_endline "     [position=(\027[01mx\027[0m,\027[01my\027[0m)]";
					print_endline "     [scale=\027[01ms\027[0m)]";
					print_endline "    Edit viewport info"
				with Scan_failure _ ->
				try sscanf cmd "quit" ();
					raise Quit
				with Scan_failure _ ->
				try sscanf cmd "print boid %i" (fun i ->
					if i < 0 || i >= n
					then print_endline "error: boid index out of bounds"
					else
						let boid = boids.(i) in
						let x, y = boid.pos in
						let vx, vy = boid.v in
						printf "Boid %i:\n" i;
						printf " - position : (%f, %f)\n" x y;
						printf " - velocity : (%f, %f)\n" vx vy;
						printf " - alive : %s\n" (if boid.alive then "yes" else "no");
						printf " - color : #%6x\n" boid.color;
						print_endline ""
				)
				with Scan_failure _ ->
				try sscanf cmd "update boid %i set%n" (fun i newcur ->
					let cur = ref newcur in
					if i < 0 || i >= n
					then print_endline "error: boid index out of bounds"
					else
					let boid = boids.(i) in
					try
						while true do
							try sscanf (stringtail cmd !cur) " position = (%f,%f)%n" (fun x y newcur ->
								cur := !cur + newcur;
								boid.pos <- x, y;
								printf "Position of boid %i set to (%f,%f)\n" i x y
							)
							with Scan_failure _ ->
							try sscanf (stringtail cmd !cur) " velocity = (%f,%f)%n" (fun x y newcur ->
								cur := !cur + newcur;
								boid.v <- x, y;
								printf "Velocity of boid %i set to (%f,%f)\n" i x y
							)
							with Scan_failure _ ->
							try sscanf (stringtail cmd !cur) " alive = %s%n" (fun str newcur ->
								cur := !cur + newcur;
								let str = String.lowercase str in
								if str = "yes" || str = "true"
								then (
									boid.alive <- true;
									printf "Boid %i set to alive\n" i
								)
								else if str = "no" || str = "false"
								then (
									boid.alive <- false;
									printf "Boid %i set to dead\n" i
								)
								else
									printf "error: `%s` is not a valid option\n" str
								
							)
							with Scan_failure _ ->
							try sscanf (stringtail cmd !cur) " color = (%i,%i,%i)%n" (fun r g b newcur ->
								cur := !cur + newcur;
								let c = rgb r g b in
								boid.color <- c;
								printf "Color of boid %i set to #%6x\n" i c
							)
							with Scan_failure _ ->
								print_endline "error: property not found";
								raise End_of_file
						done
					with End_of_file -> ()
				)
				with Scan_failure _ ->
				try sscanf cmd "update viewport set%n" (fun newcur ->
					let cur = ref newcur in
					try
						while true do
							try sscanf (stringtail cmd !cur) " position = (%f,%f)%n" (fun x y newcur ->
								cur := !cur + newcur;
								viewport_x := x;
								viewport_y := y;
								printf "Viewport position set to (%f,%f)\n" x y
							)
							with Scan_failure _ ->
							try sscanf (stringtail cmd !cur) " scale = %f%n" (fun scale newcur ->
								cur := !cur + newcur;
								viewport_scale := scale;
								printf "Viewport scale set to %f\n" scale
							)
							with Scan_failure _ ->
								print_endline "error: property not found";
								raise End_of_file
						done
					with End_of_file -> ()
				)
				with Scan_failure _ ->
					print_endline "error: command not found"
			with End_of_file -> ()
		done
	with Quit -> ()

let main () =
	ignore (Thread.create simu ());
	cli ()


let () = Arg.parse
	["--dump", Arg.Set_string dump_file,
	 "dump the data generated by the simulation in the given file.";
	 "-n", Arg.Set_int nb_cycles,
	 "number of cycles to run"]
	(fun _ -> ())
	"Usage : display.\n\
	Display a multi-agent boids-based simulation.\n\
	The available options are :"
	;
	main ()





