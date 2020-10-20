
(*Problem statement:
Q2.  Write a function zip: 'a list -> 'b list -> ('a * 'b) list, which given two lists (which should be of equal length),  
returns a list of pairs of corresponding elements of the input list, raising an exception UnequalLengths if the input 
lists are not of equal length.  What is its time and space complexity?
*)

(*Question 2:*)

exception UnequalLengths;;
let rec zip_cal l1 l2 res =
	match l1 with 
	| []-> List.rev res
	| h1::t1 -> 
	(
		match l2 with 
		| [] -> List.rev res	
		| h2::t2 -> zip_cal t1 t2 ((h1,h2)::res)
	)
;;

(*caller function *)
let zip l1 l2 = 
	if not(List.length l1 = List.length l2) then 
		raise UnequalLengths
	else
		zip_cal l1 l2 []
;;

(*Time Complexity:
There are mainly 3 main operations involved
List.Length O(n)
List.rev  - O(n)
Cons (::) - O(1) 
Therefore, over time Complexity is O(n)

Space Complexity:
Res is a list which holds all the resulted pairs, hence S.C is also O(n)
*)