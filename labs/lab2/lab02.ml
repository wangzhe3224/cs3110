let inc x = x + 1
let rec fact n = if n=0 then 1 else n*fact (n-1)

let rec fib n = if n=1 then 1 else if n=2 then 2 else fib (n-1) + fib (n-2)

let rec h n pp p = if n=1 then p else h (n-1) p (pp+p)

let fast_fib n = h n 0 1
(* 8030 will have negative fast_fib*)

let id x = x
let f x = if x then x else x  (* bool -> bool *)

let divide ~numerator:(arg1:float) ~demoninator:(arg2:float) = arg1/.arg2

let ($$) x y = (x+.y)/.2.0

let rec print_int_list = function
    | [] -> ()
    | h::t -> print_int h; print_string " "; print_int_list t

let rec sum' xs = 
    match xs with
    | [] -> 0
    | x::t -> x + sum' t

let empty' lst = 
    lst = []
