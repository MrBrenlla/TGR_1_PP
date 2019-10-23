open Big_int;;

let fib0 iteracions = let rec aux x y n = 
	if n=0 then x
		else aux (add_big_int x y) x (n-1)
			in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;
			
let fib_11 = fib0 11;;
let fib_10 = fib0 10;;
let fib_9 = fib0 9;;
			
let fib1 iteracions = let rec aux x y n =
	if n>10 then aux ( add_big_int(mult_big_int  fib_11  x)  (mult_big_int fib_10 y)) (add_big_int(mult_big_int fib_10 x)  (mult_big_int fib_9 y)) (n-10)
		else if n=0 then x
			else aux (add_big_int x y) x (n-1)
				in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;
				
let fib_101 = fib1 101;;
let fib_100 = fib1 100;;
let fib_99 = fib1 99;;


let fib2 iteracions = let rec aux x y n =
	if n>100 then aux (add_big_int(mult_big_int fib_101  x)  (mult_big_int fib_100  y)) (add_big_int(mult_big_int fib_100 x)  (mult_big_int fib_99 y)) (n-100)
		else if n>10 then aux ( add_big_int(mult_big_int  fib_11  x)  (mult_big_int fib_10 y)) (add_big_int(mult_big_int fib_10 x)  (mult_big_int fib_9 y)) (n-10)
			else if n=0 then x
				else aux (add_big_int x y) x (n-1)
				in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;

let fib_1001 = fib2 1001;;
let fib_1000 = fib2 1000;;
let fib_999 = fib2 999;;

let fib3 iteracions = let rec aux x y n =
	if n>1000 then aux (add_big_int(mult_big_int fib_1001  x)  (mult_big_int fib_1000  y)) (add_big_int(mult_big_int fib_1000 x)  (mult_big_int fib_999 y)) (n-1000)
		else if n>100 then aux (add_big_int(mult_big_int fib_101  x)  (mult_big_int fib_100  y)) (add_big_int(mult_big_int fib_100 x)  (mult_big_int fib_99 y)) (n-100)
			else if n>10 then aux ( add_big_int(mult_big_int  fib_11  x)  (mult_big_int fib_10 y)) (add_big_int(mult_big_int fib_10 x)  (mult_big_int fib_9 y)) (n-10)
				else if n=0 then x
					else aux (add_big_int x y) x (n-1)
					in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;


let fib_10001 = fib3 10001;;
let fib_10000 = fib3 10000;;
let fib_9999 = fib3 9999;;


let fib4 iteracions = let rec aux x y n =
	if n>10000 then aux (add_big_int(mult_big_int fib_10001  x)  (mult_big_int fib_10000  y)) (add_big_int(mult_big_int fib_10000 x)  (mult_big_int fib_9999 y)) (n-10000)
		else if n>1000 then aux (add_big_int(mult_big_int fib_1001  x)  (mult_big_int fib_1000  y)) (add_big_int(mult_big_int fib_1000 x)  (mult_big_int fib_999 y)) (n-1000)
			else if n>100 then aux (add_big_int(mult_big_int fib_101  x)  (mult_big_int fib_100  y)) (add_big_int(mult_big_int fib_100 x)  (mult_big_int fib_99 y)) (n-100)
				else if n>10 then aux ( add_big_int(mult_big_int  fib_11  x)  (mult_big_int fib_10 y)) (add_big_int(mult_big_int fib_10 x)  (mult_big_int fib_9 y)) (n-10)
					else if n=0 then x
						else aux (add_big_int x y) x (n-1)
						in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;
			
let fib_100001 = fib4 100001;;
let fib_100000 = fib4 100000;;
let fib_99999 = fib4 99999;;


let fib5 iteracions = let rec aux x y n =
	if n>100000 then aux (add_big_int(mult_big_int fib_100001  x)  (mult_big_int fib_100000  y)) (add_big_int(mult_big_int fib_100000 x)  (mult_big_int fib_99999 y)) (n-100000)
		else if n>10000 then aux (add_big_int(mult_big_int fib_10001  x)  (mult_big_int fib_10000  y)) (add_big_int(mult_big_int fib_10000 x)  (mult_big_int fib_9999 y)) (n-10000)
			else if n>1000 then aux (add_big_int(mult_big_int fib_1001  x)  (mult_big_int fib_1000  y)) (add_big_int(mult_big_int fib_1000 x)  (mult_big_int fib_999 y)) (n-1000)
				else if n>100 then aux (add_big_int(mult_big_int fib_101  x)  (mult_big_int fib_100  y)) (add_big_int(mult_big_int fib_100 x)  (mult_big_int fib_99 y)) (n-100)
					else if n>10 then aux ( add_big_int(mult_big_int  fib_11  x)  (mult_big_int fib_10 y)) (add_big_int(mult_big_int fib_10 x)  (mult_big_int fib_9 y)) (n-10)
						else if n=0 then x
							else aux (add_big_int x y) x (n-1)
							in aux (big_int_of_string "0") (big_int_of_string "1") iteracions;;
			

			
let crono f x =
	let t = Sys.time() in
	let _ = f x 
	in Sys.time()-.t;;


let x = Sys.argv.(1) in print_endline(string_of_big_int(fib5 (int_of_string x)));;
let x = Sys.argv.(1) in print_endline("Con un tempo de " ^ (string_of_float (crono fib5 (int_of_string x))) ^ " segundos");;

