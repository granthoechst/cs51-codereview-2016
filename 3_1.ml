(********************)
(** times Version 1**)
(********************)
let times_single1 (lst: int list) (n: int) =
	let lst2 = List.map lst ~f:(fun x -> x * n) in
	let rec times_single_rec (lst: int list) (c: int) : int =
	  match lst with
	  | [] -> 0
	  | hd::tl -> hd * (Int.pow base c) + times_single_rec tl (c+1) in
	times_single_rec (List.rev(lst2)) 0
;;

let rec times_double1 lst lst2 c =
 match lst2 with
 | [] -> 0
 | hd::tl -> (Int.pow base c)*(times_single lst hd) + (times_double lst tl (c+1))
;;

let times1 (b1 : bignum) (b2 : bignum) : bignum =
 let num = fromInt(times_double b1.coeffs (List.rev(b2.coeffs)) 0) in
 if b1.neg || b2.neg = true then negate num
 else num
;;


(********************)
(** times Version 2**)
(********************)
let times2 (b1 : bignum) (b2 : bignum) : bignum =
	if ((b1.coeffs = []) || (b2.coeffs = [])) then
   		{neg = false; coeffs = []}
	else
	let b1_neg_bool = if b1.neg then 1 else 0 in
	let b2_neg_bool = if b2.neg then 1 else 0 in
	let sign = ((b1_neg_bool lxor b2_neg_bool) = 1) in
	let b_1 = {neg = true; coeffs = b1.coeffs} in
	let b_2 = {neg = true; coeffs = b2.coeffs} in
	let b_1_rev = List.rev b_1.coeffs in
	let b_2_rev = List.rev b_2.coeffs in
	let pos_int_to_list (n : int) : int list =
	 (fromInt n).coeffs
	in
	let rec zeroes_lst (num : int) : int list =
	 match num with
	 | 0 -> []
	 | 1 -> [0]
	 | a -> 0::(zeroes_lst (num - 1))
	in
	let rec part
	   (rev_lst : int list) (digit : int) (carry : int) (zeroes : int)
	   : int list =
	 match rev_lst with
	 | [] -> (List.rev (pos_int_to_list carry))
	 | hd::tl ->
	   let res = carry + digit*hd in
	   if res < base then
	     res::(part tl digit 0 zeroes)
	   else
	     let next_carry = (res / base) in
	     let next_val = (res mod base) in
	     next_val::(part tl digit next_carry zeroes)
	in
	let rec sum (lsts : (int list) list) : int list =
	 match lsts with
	 | [] -> []
	 | [l] -> l
	 | hd::tl ->
	   (plus {neg = false; coeffs = hd}
	      {neg = false; coeffs = (sum tl)}).coeffs
	in
	let rec exec (rev_lst_b1 : int list) (rev_lst_b2 : int list) (iter : int)
	   : (int list) list =
	 match rev_lst_b2 with
	 | [] -> []
	 | [l] -> [(zeroes_lst iter) @ (part rev_lst_b1 l 0 iter)]
	 | hd::tl ->
	   (zeroes_lst iter @ (part rev_lst_b1 hd 0 iter)) ::
	     (exec rev_lst_b1 tl (iter + 1))
	in
	let reverseds = List.map ~f:(List.rev) (exec b_1_rev b_2_rev 0) in
	{neg = sign; coeffs = (sum reverseds)}
;;


(********************)
(** times Version 3**)
(** Staff Solution **)
(********************)

let times (b1: bignum) (b2: bignum) : bignum =
  let rec times_int (b: int list) (n: int) (carry: int) : int list =
    match b with
      | [] -> [carry]
      | h::t -> 
	let prod = h * n + carry in
	(prod mod base)::(times_int t n (prod / base))
  in
  let rec times_helper l1 l2 (zeroes: int list) (r: bignum) : bignum = 
    match l2 with
      | [] -> r
      | h::t -> 
	let term = List.rev (zeroes @ (times_int l1 h 0)) in
	times_helper l1 t (0::zeroes) (plus r {neg = false; coeffs = term})
  in
  let sign = not (b1.neg = b2.neg) && not (b1.coeffs = [] || b2.coeffs = []) in
  let rev1, rev2 = (List.rev b1.coeffs, List.rev b2.coeffs) in
  {neg = sign; coeffs = (times_helper rev1 rev2 [] (fromInt 0)).coeffs}
;;
