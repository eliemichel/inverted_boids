(*
	Invert tente à partir des données de sortie d'une simulation de retrouver
	les paramètres qui l'ont conduit.
*)

open Engine

let _ = Random.init (truncate (Unix.time ()))

(* dérivées partielles *)

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

(* les trois à la fois *)

let dsigmoid alpha lambda x =
	let d = x -. lambda in
	let e = exp (alpha *. d) in
	let e1 = 1. +. e in
	let f = e /. (e1 *. e1) in
	let fa = alpha *. f in
	(-. d *. f), fa, (-. fa)

(* valeur, dérivées par rapport à alpha et lambda *)

let useful_sigmoid alpha lambda x =
	let d = x -. lambda in
	let e = exp (alpha *. d) in
	let e1 = 1. +. e in
	let f = e /. (e1 *. e1) in
	let aux x = if classify_float x = FP_nan then 0. else x in
	((1. /. e1), aux (-. d *. f), aux (alpha *. f))

let random_float a b = Random.float (b -. a) +. a

let init_matrix n m f = Array.init n (fun i-> Array.init m (f i))

let sum_m m1 m2 =
	for i = 0 to Array.length m1 - 1 do
		for j = 0 to Array.length m1.(0) - 1 do
			m1.(i).(j) <- m1.(i).(j) +. m2.(i).(j)
		done
	done

let sum_v v1 v2 =
	for i = 0 to Array.length v1 - 1 do
		v1.(i) <- v1.(i) +. v2.(i)
	done
	
let mul_m c m1 =
	for i = 0 to Array.length m1 - 1 do
		for j = 0 to Array.length m1.(0) - 1 do
			m1.(i).(j) <- c *. m1.(i).(j)
		done
	done

let mul_v c v1 =
	for i = 0 to Array.length v1 - 1 do
		v1.(i) <- v1.(i) *. c
	done

let prod m1 m2 =
	init_matrix (Array.length m1) (Array.length m2.(0)) (fun i j ->
		let res = ref 0. in
		for k = 0 to  Array.length m2 - 1 do
			res := !res +. m1.(i).(k) *. m2.(k).(j)
		done;
		!res
	)
	
	
let print_mat m =
	let n = Array.length m in
	let p = Array.length m.(0) in
	for i = 0 to n-1 do
		Printf.printf "| ";
		for j = 0 to p -1 do
			Printf.printf "%f" m.(i).(j);
			Printf.printf " "
		done;
		Printf.printf "|\n"
	done
	
let inv_m m =
	let n = Array.length m in
	let m = init_matrix n (2*n) (fun i j ->
		if j < n then m.(i).(j)
		else if j = i + n then 1.
		else 0.) in
	let rec loop p =
		if p < n then (
			let k = ref p in
			while !k < n && m.(!k).(p) = 0. do incr k done;
			if !k = n then assert false;
			let tmp = m.(!k).(p) in
			for i = 0 to 2 * n - 1 do
				m.(!k).(i) <- m.(!k).(i) /. tmp
			done;
			for i = 0 to 2 * n - 1 do
				let tmp = m.(!k).(i) in
				m.(!k).(i) <- m.(p).(i);
				m.(p).(i) <- tmp
			done;
			for i = 0 to n - 1 do
				if i <> p then
					let tmp = m.(i).(p) in
					for j = 0 to 2 * n - 1 do
						m.(i).(j) <- m.(i).(j) -. tmp *. m.(p).(j)
					done
			done;
			loop (p+1)
		) in
	loop 0;
	init_matrix n n (fun i j -> m.(i).(j+n))

let check_nan s m =
	for i = 0 to Array.length m - 1 do
		for j = 0 to Array.length m.(0) - 1 do
			if classify_float m.(i).(j) = FP_nan then
				Printf.eprintf "%s : %d %d !!!!!!!!!!!!!!!!!!\n" s i j
		done
	done

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
    
(*

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

*)

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
(*
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
	let t = Array.init n (fun i -> Random.int (Array.length data - 1)) in
(*	let t = Array.init (Array.length data - 1) (fun x -> x) in
	let n = Array.length t in *)
	let grad,cost = sumt1 k n
		(fun i -> calc_grad_single2 data pre_calc param t.(i)) in
	let nf  = float n in
	grad *** (1. /. nf), cost /. nf
	
let apply_grad2 k eta n data pre_calc param =
	let grad,cost = calc_grad2 k n data pre_calc param in
	(param +++ (grad *** (-. eta /. ((float (Array.length data.(0)))
		*. (float (Array.length data))))), cost)
*)

let matrix data =
	let pre_calc = pre_calc2 data in
	let q = Array.make_matrix 5 5 0. in
	let a = Array.make 5 0. in
	let c = ref 0. in
	for i = 0 to Array.length pre_calc - 1 do
		for j = 0 to Array.length pre_calc.(0) - 1 do
			let sum_c, sum_r, sum_a, vr = pre_calc.(i).(j) in
			let sv = stay_v data.(i).(j+1).pos in
			let dv = vr ** rmi0 -- data.(i+1).(j+1).v in
			let tmp = [|sum_c; sum_r; sum_a; data.(i).(j+1).v; sv|] in
			let tmp2 = init_matrix 5 5 (fun k l -> scalar tmp.(k) tmp.(l)) in
			sum_m q tmp2;
			let tmp2 = Array.init 5 (fun k -> 2. *. (scalar tmp.(k) dv)) in
			sum_v a tmp2;
			c := !c +. (norm2 dv)
		done
	done;
	q,a,!c

let res data =
	let q,a,c = matrix data in
	let a_m = Array.init (Array.length a) (fun i -> [|(-0.5) *. a.(i)|]) in
	let iq = inv_m q in
	let s = prod iq a_m in
	Array.init (Array.length s) (fun i -> s.(i).(0))

(**
	Résolution dans le cas où tous les paramètres peuvent flotter.
	Les paramètres sont :
		cm
		ca
		cl
		rm
		ra
		rl
		am
		aa
		al : float array array
		im
		sm : float array
*)

let calc_grad_single3 data (cm,ca,cl,rm,ra,rl,am,aa,al,im,sm) i =
	let boids = data.(i) in
	let n = Array.length boids in
	let d2 = init_matrix n n (fun i j ->
		norm2 (boids.(i).pos -- boids.(j).pos)) in
	let d = init_matrix n n (fun i j -> sqrt d2.(i).(j)) in
	
	let v = Array.make n zero in
	
	let gcm = Array.make_matrix n n zero in
	let gca = Array.make_matrix n n zero in
	let gcl = Array.make_matrix n n zero in
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			let s,da,dl = useful_sigmoid ca.(i).(j) cl.(i).(j) d.(i).(j) in
			let vec =
				if d.(i).(j) = 0. then zero
				else (boids.(j).pos -- boids.(i).pos) // d.(i).(j) in
			gcm.(i).(j) <- vec ** s;
			gca.(i).(j) <- vec ** (da *. cm.(i).(j));
(*			let a,b = vec in
			let c,d = gca.(i).(j) in
			Printf.eprintf "%d %d %e %e %e %e %e\n" i j a b c d da; *)
			gcl.(i).(j) <- vec ** (dl *. cm.(i).(j));
			v.(i) <- v.(i) ++ (gcm.(i).(j) ** cm.(i).(j))
		done
	done;
	
	let grm = Array.make_matrix n n zero in
	let gra = Array.make_matrix n n zero in
	let grl = Array.make_matrix n n zero in
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			let s,da,dl = useful_sigmoid ra.(i).(j) rl.(i).(j) d.(i).(j) in
			let vec =
				if d2.(i).(j) = 0. then zero
				else (boids.(i).pos -- boids.(j).pos) // d2.(i).(j) in
			grm.(i).(j) <- vec ** s;
			gra.(i).(j) <- vec ** (da *. rm.(i).(j));
			grl.(i).(j) <- vec ** (dl *. rm.(i).(j));
			v.(i) <- v.(i) ++ (grm.(i).(j) ** rm.(i).(j))
		done
	done;
	
	let gam = Array.make_matrix n n zero in
	let gaa = Array.make_matrix n n zero in
	let gal = Array.make_matrix n n zero in
	for i = 0 to n - 1 do
		for j = 0 to n - 1 do
			let s,da,dl = useful_sigmoid aa.(i).(j) al.(i).(j) d.(i).(j) in
			let vec = boids.(j).v -- boids.(i).v in
			gam.(i).(j) <- vec ** s;
			gaa.(i).(j) <- vec ** (da *. am.(i).(j));
			gal.(i).(j) <- vec ** (dl *. am.(i).(j));
			v.(i) <- v.(i) ++ (gam.(i).(j) ** am.(i).(j))
		done
	done;
	
	let gim = Array.make n zero in
	for i = 0 to n - 1 do
		gim.(i) <- boids.(i).v;
		v.(i) <- v.(i) ++ (gim.(i) ** im.(i))
	done;
	
	let gsm = Array.make n zero in
	for i = 0 to n - 1 do
		gsm.(i) <- stay_v boids.(i).pos;
		v.(i) <- v.(i) ++ (gsm.(i) ** sm.(i))
	done;
	
	let c = ref 0. in
	let dv = Array.init n (fun j -> v.(j) -- data.(i+1).(j).v) in
	
	for i = 0 to n - 1 do
		c := !c +. (norm2 dv.(i))
	done;
	
	let aux vec i = 2. *. (scalar vec dv.(i)) in
	let aux2 m = init_matrix n n (fun i j ->
		aux m.(i).(j) i) in
	let aux3 t = Array.init n (fun i -> aux t.(i) i) in
	let gcm = aux2 gcm in
	let gca = aux2 gca in
	let gcl = aux2 gcl in
	let gam = aux2 gam in
	let gaa = aux2 gaa in
	let gal = aux2 gal in
	let grm = aux2 grm in
	let gra = aux2 gra in
	let grl = aux2 grl in
	let gim = aux3 gim in
	let gsm = aux3 gsm in
	
(*	check_nan "gca-1" gca; *)
	
	((gcm,gca,gcl,grm,gra,grl,gam,gaa,gal,gim,gsm),!c)

let calc_grad3 n data param =
	let t = Array.init n (fun i -> Random.int (Array.length data - 1)) in
	let nb = Array.length data.(0) in
	
	let gcm2 = Array.make_matrix nb nb 0. in
	let gca2 = Array.make_matrix nb nb 0. in
	let gcl2 = Array.make_matrix nb nb 0. in
	let gam2 = Array.make_matrix nb nb 0. in
	let gaa2 = Array.make_matrix nb nb 0. in
	let gal2 = Array.make_matrix nb nb 0. in
	let grm2 = Array.make_matrix nb nb 0. in
	let gra2 = Array.make_matrix nb nb 0. in
	let grl2 = Array.make_matrix nb nb 0. in
	let gim2 = Array.make nb 0. in
	let gsm2 = Array.make nb 0. in
	let c2 = ref 0. in
	
	let rec loop j =
		if j >= 0 then (
			let (gcm,gca,gcl,grm,gra,grl,gam,gaa,gal,gim,gsm),c =
				calc_grad_single3 data param t.(j) in
			List.iter2 sum_m [gcm2;gca2;gcl2;grm2;gra2;grl2;gam2;gaa2;gal2]
				[gcm;gca;gcl;grm;gra;grl;gam;gaa;gal];
			List.iter2 sum_v [gim2;gsm2] [gim;gsm];
			c2 := !c2 +. c;
			loop (j-1)
		) in
	loop (n-1);
	
(*	check_nan "gca2" gca2; *)
	
	let nf = 1. /. (float n) in
	List.iter (mul_m nf) [gcm2;gca2;gcl2;grm2;gra2;grl2;gam2;gaa2;gal2];
	List.iter (mul_v nf) [gim2;gsm2];
	
	((gcm2,gca2,gcl2,grm2,gra2,grl2,gam2,gaa2,gal2,gim2,gsm2),!c2)
	
let apply_grad3 eta n data (cm,ca,cl,rm,ra,rl,am,aa,al,im,sm) =
	let ((gcm,gca,gcl,grm,gra,grl,gam,gaa,gal,gim,gsm),cost) =
		calc_grad3 n data (cm,ca,cl,rm,ra,rl,am,aa,al,im,sm) in
(*	check_nan "gca" gca; *)
	let c = (-1.) *. eta /. (float (Array.length data - 1)) in
	List.iter (mul_m c) [gcm;gca;gcl;grm;gra;grl;gam;gaa;gal];
	List.iter (mul_v c) [gim;gsm];
	List.iter2 sum_m [cm;ca;cl;rm;ra;rl;am;aa;al]
		[gcm;gca;gcl;grm;gra;grl;gam;gaa;gal];
	List.iter2 sum_v [im;sm] [gim;gsm];
	cost

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

let output = ref ""

let interact = ref false

let main () =
	let data = read_data !name in
(*	let s = res data in *)
(*	let pre_calc = pre_calc2 data in
	let init_param = [|1.;1.;1.;1.;1.|] in *)
(*	let init_param = [|0.001;10.;0.01;1.;0.25|] in *)
	let param =
		let n = Array.length data.(0) in
		let a = Array.make_matrix n n in
		let b = Array.make n in
		a 1.,
		a 1.,
		a 100.,
		a 1.,
		a 1.,
		a 20.,
		a 1.,
		a 1.,
		a 100.,
		b 1.,
		b 1. in
(*	let print t =
		let (cb,rb,ab,ib,sb) = get_coeff5 t in
		Printf.printf "cm = %f\nrm = %f\nam = %f\nim = %f\nsm = %f\n"
			cb rb ab ib sb in *)
	let print oc (cm,ca,cl,rm,ra,rl,am,aa,al,im,sm) =
		let print_mat s m =			
			Printf.fprintf oc "########\n# %-5s#\n########\n\n" s;
			Array.iter (fun r -> Array.iter (fun c ->
				Printf.fprintf oc "%12.3e" c) r;
				Printf.fprintf oc "\n") m;
			Printf.fprintf oc "\n\n" in
		let print_vect s v =
			Printf.fprintf oc "########\n# %-5s#\n########\n\n" s;
			Array.iter (fun c ->
				Printf.fprintf oc "%12.3e" c) v;
			Printf.fprintf oc "\n\n\n" in
		List.iter2 print_mat ["cm";"ca";"cl";"rm";"ra";"rl";"am";"aa";"al"]
			[cm;ca;cl;rm;ra;rl;am;aa;al];
		List.iter2 print_vect ["im";"sm"] [im;sm] in
	let rec loop = function
		| 0 -> ()
		| n ->
			let cost = apply_grad3
				(!eta /. sqrt (float (!nb_gens - n + 1)))
				!nb_grads data param in
			Printf.printf "====================\n";
			Printf.printf "n = %d\n" (!nb_gens - n);
			Printf.printf "c = %f\n%!" cost;
			let oc =
				if !output = "" then stdout
				else open_out !output in
			print oc param;
			if !output <> "" then
				close_out oc;
			if !interact then (
				Scanf.scanf "%s\n" (fun s -> ())
			);
			loop (n-1) in
	loop !nb_gens;
	let oc =
		if !output = "" then stdout
		else open_out !output in
	print oc param;
	if !output <> "" then
		close_out oc


let () = Arg.parse
	["-n", Arg.Set_int nb_gens,
		"number of cycles to run";
	"-ng", Arg.Set_int nb_grads,
		"number of instances to use when computing the gradient";
	"-l", Arg.Set_float eta,
		"learning rate";
	"-i", Arg.Set interact,
		"interactive mode";
	"-o", Arg.Set_string output,
		"output file"]
	(fun s -> name := s)
	"Usage : invert data.\n\
	Tries to recover the parameters given to a simulation from the position\n\
	outut. These data are given as first argument.\n\
	The available options are :";
	main ()

