type 'a mlist = Nil | Cons of 'a * (('a mlist) ref)

(*** Warmup -- what can be improved here quickly? ***)
(*** What could be improved here with some more work? ***)
let rec has_cycle (lst : 'a mlist) : bool =
	let rec check_cycle l visited : bool =
		match l with
		| Nil -> false
		| Cons(h, t) -> (List.fold_right (fun x acc -> x==(!t) || acc ) visited false) 
						|| check_cycle !t (lst::visited)
	in check_cycle lst []
;;

let rec flatten (lst : 'a mlist) : unit =
	let rec flatten_rec lst visited =
		match lst with 
		| Nil -> ()
		| Cons(h, t) -> if (List.fold_right (fun x acc -> x==(!t) || acc) visited false)
						then t := Nil
						else flatten_rec !t (lst::visited)
	in flatten_rec lst []
;;

let mlength (lst : 'a mlist) : int = 
	let rec mlength_rec lst visited =
		match lst with
		| Nil -> 0
		| Cons(_, t) -> if List.fold_right (fun x acc -> x ==(!t) || acc) visited false 
						then 1 
						else 1 + mlength_rec !t (lst::visited)
	in mlength_rec lst []
;;



(*** Excercise 2 ***)
open Lazy

type 'a stream = Cons of 'a * 'a stream Lazy.t

let head (s:'a stream) : 'a =
  let Cons(v,_) = s in v
;;

let merge (s1 : int stream) (s2 : int stream) : int stream =
	 let rec merge_help (s1 : int stream) (s2 : int stream)
	     (lst_hd : int) : int stream =
	   let Cons(hd1,tl1) = s1 in
	   let Cons(hd2,tl2) = s2 in
	   match hd1 > hd2 with
	   |true ->
	     if hd2 = lst_hd 
       then merge_help s1 (Lazy.force tl2) lst_hd
	     else Cons(hd2, lazy(merge_help s1 (Lazy.force tl2) hd2))  
	   |false ->
	     (match hd1 < hd2 with
	     |true ->
	       if hd1 = lst_hd 
         then merge_help (Lazy.force tl1) s2 lst_hd
	       else Cons(hd1, lazy(merge_help (Lazy.force tl1) s2 lst_hd))
	     |false ->
	       if hd1 = lst_hd 
          then merge_help (Lazy.force tl1) (Lazy.force tl2) lst_hd
	       else Cons(hd1, lazy(merge_help (Lazy.force tl1) (Lazy.force tl2) hd1))) in
	 let frst = (min (head s1) (head s2)) - 1 in
	 merge_help s1 s2 frst
;;

