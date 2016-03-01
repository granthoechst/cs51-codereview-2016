(* 
	Section 2 Code Review 
	Review of Pset 1
*)

(* Example 1 Variance *)

let rec sum a = 
	match a with 
	| [] -> 0.
	| head :: tail -> head +. sum tail
;;

let rec count a = 
	match a with
	| [] -> 0.
	| _ :: tail -> 1. +. count tail
;;

let average a = sum a /. count a;;

(* Example 1 Exercise 1 *)
(* Determine an alternative and more efficient way of finding the average 
   for a list. 
*)

let square x = x *. x;;

let rec ssqd (a : float list) (b : float) : float =
	match a with 
	| [] -> 0.
	| head :: tail -> square (head -. b) +. ssqd tail b
;;

let variance1a a =
	let mu = average a in
	let diff = ssqd a mu in
	let n = count a in
		(1. /. (n -. 1.)) *. diff
;;

let() = assert (variance1a [1.0; 2.0; 3.0; 4.0; 5.0] = 2.5);;
let() = assert (variance1a [1.0; 1.0; 1.0] = 0.0);;

let variance2 a =
	let rec sum a = 
		match a with 
		| [] -> 0.
		| head :: tail -> head +. sum tail in
	let rec count a = 
		match a with
		| [] -> 0.
		| _ :: tail -> 1. +. count tail in
	let average a = sum a /. count a in
	let square x = x *. x in
	let rec ssqd (a : float list) (b : float) : float =
		match a with 
		| [] -> 0.
		| head :: tail -> square (head -. b) +. ssqd tail b in
	let mu = average a in
	let diff = ssqd a mu in
	let n = count a in
		(1. /. (n -. 1.)) *. diff
;;

let rec list_length = function
   | [] -> 0
   | hd::tl -> 1 + list_length tl
;;

let rec list_reduce ~f ~u = function
   | [] -> u
   | hd::tl -> f hd (list_reduce ~f ~u tl)
;;

let rec list_map ~f = function
   | [] -> []
   | hd::tl -> (f hd)::(list_map ~f tl)
;;

let variance3 (lst : float list) : float option =
 let n = list_length lst in
 if n < 2 then None
 else
   let summate = list_reduce ~f:(+.) ~u:0. in
   let sum = summate lst in
   let mean = sum /. (float n) in
   let result = (1. /. (float (n - 1))) *.
           (summate (list_map lst ~f:(fun x -> (x -. mean) ** 2.))) in
   Some result
;;

(* Example 1 Exercise 2: *)
(* Replace the code from example 2 to use the Ocaml H.O.F *)
open List;;

(* Example 2 *)

let rec to_run_length1 (lst : char list) : (int * char) list =
	 (* return a tuple of #repeitions and the remaining list ex. repeated chars *)
	 let rec remove_repeats (l : char list) : int * (char list) =
	   match l with
	   | [] -> (0,[])
	   | [_] -> (1,[])
	   | hd1::hd2::tl ->
	       if hd1 = hd2 then
	         let (num, remains) = remove_repeats (hd2::tl) in
	         (1 + num, remains)
	       else (1, hd2::tl) in
	 match lst with
	 | [] -> []
	 | hd::_ ->
	     let (num, remains) = remove_repeats lst in
	     (num, hd)::to_run_length1 remains;;
	
let rec from_run_length1 (lst : (int * char) list) : char list =
 match lst with
 | [] -> []
 | (n,c)::tl ->
     if n > 0 then c::from_run_length1 ((n-1,c)::tl) else from_run_length1 tl

let rec to_run_length2 (x: char list) : (int * char) list =
 match x with
 | [] -> []
 | xhd::xtl ->
   match to_run_length2 xtl with
     | [] -> [(1, xhd)]
     | (n, c)::tl ->
       if xhd = c then
         (n + 1, c)::tl
       else
         (1, xhd)::(n, c)::tl

let rec from_run_length2 (x: (int * char) list) : char list =
 match x with
 | [] -> []
 | (n, c)::xtl ->
     if n > 0 then
       c::from_run_length2 ((n-1,c)::xtl)
     else
       from_run_length2 xtl



