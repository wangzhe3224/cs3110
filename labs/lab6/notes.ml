(* Basic higher order functions *) 
let apply f x = f x
let pipeline x f = f x
let composee f g x = f (g x)
let both f g x = (f x, g x)
let cond p f g x = if p then f x else g x
(* fold_right *)
let rec combine op init = function
    | [] -> init
    | h::t -> op h (combine op init t)

let sum = combine (+) 0
let concat = combine (^) ""

(* fold_left is tail recursion *)
let length = List.fold_left (fun a _ -> a+1) 0 
let rev = List.fold_left (fun a x -> x::a) [] 
let map f l = List.fold_left (fun a x -> (f x)::a) [] l |> rev
let map' f l = List.fold_right (fun x a -> (f x)::a) l []
let filter f l = List.fold_right (fun x a -> if f x then x::a else a) l []

type 'a tree = 
    | Leaf
    | Node of 'a * 'a tree * 'a tree

let rec foldtree init op = function
    | Leaf -> init
    | Node (v, l, r) -> op v (foldtree init op l) (foldtree init op r)

let size t = foldtree 0 (fun x l r -> 1 + l + r) t

let square x = x*x

let (--) i j = 
    let rec from i j l = 
        if i>j then l
        else from i (j-1) (j::l)
    in from i j []
let sum_sqr n = 0--n |> List.map square |> sum
