type 'a matrix = 'a array array
type pos = float * float 
type rule = pos array -> pos array -> float matrix -> int -> pos

let zero = 0.,0.

let (++) (a,b) (c,d) = a +. c, b +. d
let (--) (a,b) (c,d) = a -. c, b -. d
let ( ** ) (a,b) c = c *. a, c *. b
let ( // ) (a,b) c = a /. c, b /. c

let norm (a,b) = a *. a +. b *. b
let ds a b = norm (a -- b)
let d a b = sqrt (ds a b)

let stay xmin xmax ymin ymax p v c i =
	let x,y = p.(i) in
	let ax =
		if x < xmin then c.(i).(i)
		else if x > xmax then -. c.(i).(i)
		else 0. in
	let ay =
		if y < ymin then c.(i).(i)
		else if y > ymax then -. c.(i).(i)
		else 0. in
	ax,ay
	
let herd radius p v c i =
	let res = ref zero in
	let s = ref 0. in
	for j = 0 to Array.length c - 1 do
		let ds = ds p.(j) p.(i) in
		if ds < radius then (
			res := !res ++ (p.(j) ** c.(i).(j));
			s := !s +. c.(i).(j)
		)
	done;
	if !s > 0 then (!res // !s) -- p.(i)s
	else zero
	
let speed radius p v c i =
	let res = ref zero in
	let s = ref 0. in
	for j = 0 to Array.length c - 1 do
		let ds = ds p.(j) p.(i) in
		if ds < radius && i <> j then (
			res := !res ++ (v.(j) ** c.(i).(j));
			s := !s +. c.(i).(j)
		)
	done;
	if !s > 0 then !res ** (c.(i).(i) /. !s)
	else zero
	
let inertia p v c i =
	v.(i) ** c.(i).(i)
	
let keep radius p v c i 
