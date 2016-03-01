open Ast
open ExpressionLibrary

(*** Example 1: contains_var ***)
let rec contains_var (e: expression) : bool = 
	match e with
	| Var -> true
	| Binop(n, a, b) ->
		(if (contains_var a = false)
		then contains_var b
		else true)
	| Unop(x, c) -> contains_var c
	| _ -> false
;;



(*** Example 1: Solution Code ***)
let rec contains_var (e : expression) : bool =
	match e with
	| Var -> true
	| Binop(_, a, b) -> contains_var a || contains_var b
	| Unop(_, c) -> contains_var c
	| _ -> false
;;



(*** End Example 1 Solution Code ***)

let rec evaluate (e:expression) (x:float) : float =
	match e with
	| Var-> x
	| Num n-> n
	| Binop (binop, exp1, exp2)->
		(match binop with
			| Add-> (evaluate exp1 x)+.(evaluate exp2 x)
		 	| Sub-> (evaluate exp1 x)-.(evaluate exp2 x)
		 	| Mul-> (evaluate exp1 x)*.(evaluate exp2 x)
			| Div-> (evaluate exp1 x)/.(evaluate exp2 x)
			| Pow-> (evaluate exp1 x)**(evaluate exp2 x))
 	| Unop(unop, exp1) -> 
 		(match unop with
			| Sin-> sin(evaluate exp1 x)
			| Cos-> cos(evaluate exp1 x)
			| Ln-> log(evaluate exp1 x)
			| Neg-> (-1.)*.(evaluate exp1 x))
;;

(* unit tests *)
assert((evaluate (parse "x + 2. + 5.") 4.0) = 11.0);;  
assert((evaluate (parse "x^2 + 2. + x") 4.0) = 22.0);;
assert((evaluate (parse "x^3 + 2. + x") 1.0) = 4.0);;

(*** Sample Exhaustive Tests ***)
(* Num path *)
assert((evaluate (parse "5.") 5.) = 5.);;
(* Var path *)
assert((evaluate (parse "x") 5.) = 5.);;
(* Binom paths *)
assert((evaluate (parse "(x * x) + (3*x) - (x * x / x)^2") 2.) = 6.);;
(* Unop paths *)
assert((evaluate (parse "~x + sin(1.-x) + cos(0) + ln(x)") 1.) = 0.);;