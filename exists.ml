(* Q1.  Write a program exists: bool list -> bool which  finds the disjunction of all the elements in 
a given list of boolean values  using standard list functions such as map, filter and  foldr.    
What is its time and space complexity? *)

let rec exists_cal l res = 
match res with 
| true -> true
| _ ->
(
	match l with
	| []  -> res
	| h::t -> 
	(
		exists_cal t (h || res)
	)
)
;;
(* Caller function *)
let exists l = 
	match  l with
	| [] -> false
	| h::t -> exists_cal l false
;;


(*Time complexity:
Worst case is O(n) because it has to check all the boolean values of the list
Best case is o(1) because as soon as it finds first truth value as true it retruns true

Space Complexity: S.C is O(1) because res is a variable used to hold the disjunction of two values.
Please note that it's a recursive program and the S.C has not considered the space used for recursive calls
which can be O(n) in worst case as there can be n recursive calls in worst case.*)
