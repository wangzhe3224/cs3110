let rec repeat f n x = 
    if n = 0 then x
    else repeat f (n-1) (f x)

let product_left lst = 
    List.fold_left (fun a x -> a*x) 1 lst

let product_right lst = 
    List.fold_right (fun x a -> a*x) lst 1

let clip n = 
    if n < 0 then 0 
    else if n > 10 then 10
    else n

let cliplist = List.map clip
let cliplist' lst = 
    match lst with
    | [] -> []
    | x::xs -> (clip x)::(cliplist xs)

let (--) i j = 
    let rec from i j l = 
        if i>j then l
        else from i (j-1) (j::l)
    in from i j []

let (>>) f g x = g (f x)

let rec combine op init = function
    | [] -> init
    | h::t -> op h (combine op init t)
let sum = combine (+) 0
let flip f x y = f y x
let sum_cube_odd lst = List.filter (fun x -> if x mod 2 = 0 then false else true) lst
                       |> List.map (fun x -> x*3)
                       |> sum

let rec exist_rec p lst = 
    match lst with
    | [] -> false
    | x::xs -> if p x then true
               else exist_rec p xs

let exist_fold p lst = List.fold_left (fun a x -> a || (p x)) false lst

let budget total expenses = List.fold_left (fun a x -> a - x) total expenses

let compose_map f g lst = List.map (fun x -> f (g x)) lst

type 'a tree = 
| Leaf 
| Node of 'a * 'a tree * 'a tree

let rec tree_map f tree = 
    match tree with
    | Leaf -> Leaf
    | Node (a,left,right) -> Node (f a, tree_map f left, tree_map f right) 

let t1 = Node (1, Leaf, Leaf)

let add1 = tree_map (fun x -> x + 1)

type vector = int list
type matrix = vector list

let is_valid_matrix m = 
    let length = List.length m in
        match m with 
        | [] -> true
        | rs -> List.fold_left (fun a x -> a && (List.length x = length)) true rs

let dot = List.map2 (fun a b -> a * b)
(* let rec transpose (m: matrix) = *) 
(*     match m with *)
(*     | [] -> [] *)
(*     | []::xss -> transpose xss *)
(*     | [x::xs]::xss -> (x::List.map List.hd xss) :: transpose (xs:: List.map List.tl xss) *)

let rec transpose list = match list with
    | []             -> []
    | []   :: xss    -> transpose xss
    | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let add_row_vector = List.map2 (fun a b -> a + b)
let add_matrices = List.map2 add_row_vector
let mul m1 m2 = List.map2 dot m1 (transpose m2) 

let m1 = [[1;2];[3;4]]
let m3 = [[1;2];[3;4]]
let m2 = [[1;]; [2;3]]
let v1 = [1;2;3]
let v2 = [1;2;3]
let m1_m3 = mul m1 m3
