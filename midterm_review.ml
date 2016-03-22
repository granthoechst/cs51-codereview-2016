(*Midterm Review*)



let inc = (fun x -> x + 1) in
inc 6

(fun x -> x + 1) 6;;



let add x y = x + y in
let inc = add 1 in
let dec = add (-1) in
dec(inc 7)

add (-1) (add 1 7);;



let a = 3 in
let a = 5 in
a + a



let a = 30 in
let a = (let a = 3 in a * 4) in
a + a

24



(* 2015 3b *)
let f h g = g (h 42) +. 51.0


(int -> 'a) -> ('a -> float) -> float






(* 2015 3d *)
let rec f x y = 
	match x with
	| Some (v, w) -> [v + w; f w] 
  	| None -> f y








(* 2012 1d *)
let f g h = fun x -> h (g x) in 
let f = f (fun x -> (x, x)) in 
let f = f (fun x -> let (x, _) = x in x + x) in 
f 3 
