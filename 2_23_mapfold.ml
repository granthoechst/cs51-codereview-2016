(* Mathematics *)
(* Contains var examples *)

(*** Example 1: Deoptionalize ***)

let deoptionalize = map ???

(*

let deoptionalize0 (lst: 'a option list) : 'a list =
	let filter (num : 'a option) : bool =
	  match num with
	  | None -> false
	  | _ -> true in
	let filtered_lst = List.filter filter lst in	 
	let conversion (num: 'a option) : 'a =
	  match num with
	  | Some(value) -> value in
	List.map conversion filtered_lst
;;

(*

let deoptionalize1 (lst:'a option list) : 'a list =
	List.map (fun x-> match x with | Some n -> n)
			 (List.filter (fun x-> x <> None) lst);;

(*

let deoptionalize2 (lst:'a option list) : 'a list =  
	let filter (num : 'a option) : bool =
	  match num with
	  | None -> false
	  | _ -> true in
	let filtered_lst = List.filter filter lst in	 
	let conversion (num: 'a option) : 'a =
	  match num with
	  | Some(value) -> value
	  | None -> failwith "Error during filtering" in
	List.map conversion filtered_lst
;;

(*

let deoptionalize3 (lst:'a option list) : 'a list = 
	List.map (fun x ->
		match x with
		| Some a -> a
		| None -> failwith "Got None!")
	(List.filter (fun x -> x <> None) lst);;

(*

let deoptionalize4 (lst: 'a option list) : 'a list = 
	List.fold_right (fun x acc -> match x with
									| Some n -> n::acc
									| None -> acc) lst []
;;





(*** Example 2: Concat, Sum, and Sum Rows ***)
let concat1 (lists : 'a list list) : 'a list =
	let reduce l1 l2 = l1 @ l2 in
	List.fold_right reduce lists []
;;

let concat2 (lists : 'a list list) : 'a list = 
	List.fold_right (@) lists []
;;

let sum1 (nums : int list) : int =
	let add a b = a + b in
	List.fold_right add nums 0
;;

let sum_rows1 (rows : int list list) : int list =
	let f (l:int list) : int = sum l in 
		List.map (fun l->f l) rows

(*** Solution Code ***)
let sum2 (nums : int list) : int = 
	List.fold_right (+) nums 0
;;

let sum_row2 (rows : int list list) : int list = 
	List.map sum rows 
;;
(*** End Example 2 Solution Code ***)

(*** Example 3: num_occurs, filter_range, and filter_by_year ***)
let num_occurs1 (n:int) (nums:int list) : int =
	let find_number (l:int list) : int list =
		List.filter (fun x -> x = n) l in 
	let make_ones (list2 : int list) : int list =
       List.map (fun n -> n/n) list2 in  
   	sum (make_ones (find_number nums)) 
;;

let num_occurs2 (n : int ) (nums : int list) : int =
	List.fold_right (fun x acc -> if x = n then acc + 1 else acc) nums 0
;;

let filter_range1 (nums : int list) (range : int * int) : int list = 
	let too_high (list:int list) : int list =
	    List.filter ~f:(fun x -> x <= snd range) list in
	let too_low (list:int list) : int list =
	    List.filter ~f:(fun x -> x >= fst range) list in
	too_high (too_low (nums)) 

let filter_range2 (nums : int list ) (range : int * int) : int list =
	List.filter (fun a -> 
		let (x,y) = range in 
		((a <= x && a >= y) || (a >= x && a <= y ))) nums

(*** Solution Code ***)
let filter_range3 (nums : int list) (range : int * int) : int list =
	List.filter (fun a -> let (x,y) = range in x <= a && a <= y) nums
;;
(*** End Solution Code ***)

