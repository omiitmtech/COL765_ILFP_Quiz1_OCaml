
(*Problem statement:
Q3. Implement  dotprod: float list * float list -> float which computes the scalar or dot product  of two vectors 
(written as two float lists) using map, zip, foldr.    What is its time and space complexity?
*)

(*Question 2:*)

exception UnequalLengths;;
let rec dotprod_cal l1 l2 res =
	match l1 with 
	| []-> res
	| h1::t1 -> 
	(
		match l2 with 
		| [] -> res	
		| h2::t2 -> dotprod_cal t1 t2 (h1*.h2 +. res)
	)
;;

(*caller function *)
let dotprod l1 l2 = 
	if not(List.length l1 = List.length l2) then 
		raise UnequalLengths
	else
		dotprod_cal l1 l2 0.0
;;

(*Time Complexity:
There are mainly 3 main operations involved
List.Length O(n)
Cons (::) - O(1) 
Therefore, over all time Complexity is O(n), assuming that mutiplication/addition takes constant time

Space Complexity:
Space Complexity: S.C is O(1) because res is a variable used to hold the final sum.
Please note that it's a recursive program and the S.C has not considered the space used for recursive calls
which can be O(n) in worst case as there can be n recursive calls in worst case.
*)
