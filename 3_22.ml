(*
	Section 5
	Problem Set 4 Review
*)


(* BinSTree*)

(* Example 1 *)
let rec search1 (x : elt) (t : tree) : bool =
match t with
| Leaf -> false
| Branch(_, [], _) -> failwith "Invalid tree: empty list as node"
| Branch(l,hd::_,r) -> (match C.compare x hd with
                         | Equal -> x = hd
                         | Less -> search x l
                         | Greater -> search x r)

let rec search2 (x : elt) (t : tree) : bool =
   match t with
   | Leaf -> false
   | Branch (l, v, r) ->
     (match v with
     | [] -> failwith "Invalid tree: empty list as node"
     | hd :: _ ->
       (match C.compare x hd with
       | Equal -> List.fold_right (fun x r -> (hd=x) || r) v false
       | Greater -> search x r
       | Less -> search x l))

let rec search3 (x : elt) (t : tree) : bool =
   match t with
   | Leaf -> false
   | Branch (l, v, r) ->
     (match v with
     | [] -> failwith "Invalid tree: empty list as node"
     | hd :: _ as lst ->
       (match C.compare x hd with
       | Equal -> List.mem x lst
       | Greater -> search x r
       | Less -> search x l))

(* Example 2*)
let rec getmin1 (t : tree) : elt =
  match t with
  | Leaf -> raise EmptyTree
  | Branch (Leaf, v, _) ->
    (match List.rev v with
    | [] -> failwith "Invalid tree: empty list as node"
    | hd::_ -> hd)
  | Branch (l, _, _) -> getmin l

let rec getmax1 (t : tree) : elt =
  match t with
  | Leaf -> raise EmptyTree
  | Branch (_, v, Leaf) ->
    (match List.rev v with
    | [] -> failwith "Invalid tree: empty list as node"
    | hd::_ -> hd)
  | Branch (_, _, r) -> getmax r

(*Exercise 1: Modify get_min and get_max so that repeated code is
  factored out of the function*)


(* Priority Queues *)
(* Example 3 *)
let rec add1 (e : elt) (q : queue) =
   match q with
   | [] -> [e]
   | hd::tl ->
       match C.compare e hd with
       | Equal | Greater -> e::q
       | Less -> hd::(add e tl)

let take1 (q : queue) = 
	match List.rev q with
	   | [] -> raise QueueEmpty
	   | hd::tl -> (hd, List.rev tl)

let rec add2 (e : elt) (q : queue) =
    match q with
    | [] -> [e]
    | hd::tl ->
      match C.compare e hd with
      | Less -> e::q
      | Greater | Equal -> hd::(add e tl)

let take2 (q : queue) =
    match q with
    | [] -> raise QueueEmpty
    | hd::tl -> hd, tl

(* Binary Heap *)
(* Example 4 *)
let get_top (t : tree) : elt =
   match t with
   | Leaf e
   | OneBranch (e,_)
   | TwoBranch (_,e,_,_) -> e

let replace_root (e: elt) (t: tree) : tree =
   match t with
   | Leaf _ -> Leaf e
   | OneBranch (_,e2) -> OneBranch (e,e2)
   | TwoBranch (bal,_,t1,t2) -> TwoBranch (bal,e,t1,t2)

let rec fix (t : tree) : tree =
	match t with
	| Leaf _ -> t
	| OneBranch (e1,e2) ->
	  (match C.compare e1 e2 with
	  | Less -> t
	  | Equal | Greater -> OneBranch (e2,e1))
	| TwoBranch (bal,e,t1,t2) ->
	  let top1 = get_top t1 in
	  let top2 = get_top t2 in
	  match C.compare top1 top2 with
	  | Less | Equal ->
	      (match C.compare e top1 with
	      | Less -> t
	      | Equal | Greater ->
	          TwoBranch (bal,top1,fix (replace_root e t1),t2))
	  | Greater ->
	      (match C.compare e top2 with
	      | Less -> t
	      | Equal | Greater ->
	          TwoBranch (bal,top2,t1,fix (replace_root e t2)))

	
