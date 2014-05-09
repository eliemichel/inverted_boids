
open Graphics
open Engine
open Scanf
open Printf

exception Quit



let stringtail str i =
	String.sub str i ((String.length str) - i)



let rec eval rules boids viewport_x viewport_y viewport_scale cmd =
	let n = Array.length boids in
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
		print_endline "    Edit viewport info";
		print_endline " - source \027[01mfilename\027[0m: execute \027[01mfilename\027[0m content as standard CLI input"
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
	try sscanf cmd "update rule %i -> %i set%n" (fun i j newcur ->
		let cur = ref newcur in
		if i < 0 || i >= n
		then print_endline "error: first boid index out of bounds"
		else
		if j < 0 || j >= n
		then print_endline "error: second boid index out of bounds"
		else
		let boid1 = boids.(i) in
		let boid2 = boids.(j) in
		try
			while true do
(*
Cohesion (cm,ca,cl);
Repulsion (rm,ra,rl);
Alignment (am,aa,al);
Inertia im;
Stay sm
*)
				try sscanf (stringtail cmd !cur) " cohesion = %f%n" (fun c newcur ->
					cur := !cur + newcur;
					(match rules with
					| Cohesion (cm,_,_) :: _ -> cm.(i).(j) <- c
					| _ -> failwith "Rules order has been changed and so it broke the CLI !"
					);
					printf "Cohesion of boid %i toward boid %i set to %f\n" i j c
				)
				with Scan_failure _ ->
					print_endline "error: property not found";
					raise End_of_file
			done
		with End_of_file -> ()
	)
	with Scan_failure _ ->
	try sscanf cmd "source %s" (fun filename ->
		try
			let f = open_in filename in
			try
				while true do
					let cmd' = input_line f in
					printf "> \027[01m%s\027[0m\n" cmd';
					eval rules boids viewport_x viewport_y viewport_scale cmd'
				done;
			with Quit | End_of_file ->
				close_in f
		with Sys_error err ->
			printf "error: %s\n" err
	)
	with Scan_failure _ ->
		print_endline "error: command not found"

