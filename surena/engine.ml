(*
	Engine exécute la simulation du systeme et définit des types et procédures
	permettant sa manipulation.
*)


type boid = {
	pos   : float * float;
	v     : float * float;
	alive : bool; (* true = agent ; false = Décors *)
	color : color
}

let zero = (0.,0.)

let norm2 (x,y) = x *. x +. y *. y

let (++) (a,b) (c,d) = ((a+.c),(b+.d))
let (--) (a,b) (c,d) = ((a-.c),(b-.d))
let ( // ) (a,b) n = ((a/.n),(b/.n))
let ( ** ) (a,b) n = ((a*.n),(b*.n))

let d a b = norm2 (a -- b)

let random_pos xmin xmax ymin ymax =
	(xmin +. (Random.float (xmax -. xmin))),
	(ymin +. (Random.float (ymax -. ymin)))



let step rules positions =
	(**
		`step rules positions`
		met à jour le vecteur de positions à partir des règles données.
	*)
	()



