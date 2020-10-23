(* Using variants, we can express a type that represents the union of several other types, but in a type-safe way. *)
type point = float*float
type vector = float list
type matrix = float list list
type shape = 
    | Point of point
    | Circle of point * float
    | Rect of point * point

let area = function
    | Point _ -> 0.0
    | Circle (_, r) -> 3.14159 *. (r**2.0)
    | Rect ((x1, y1), (x2, y2)) -> 
            let w = x2 -. x1 in
            let h = y2 -. y1 in
                w *. h

let center = function
    | Point p -> p
    | Circle (p, _) -> p
    | Rect ((x1, y1), (x2, y2)) -> 
          ((x2 -. x1) /. 2.0, (y2 -. y1) /. 2.0)

type string_or_int = 
    | String of string
    | Int of int
type string_or_int_list = string_or_int list

let rec sum : string_or_int_list -> int = function
    | [] -> 0
    | (String s)::t -> int_of_string s + sum t
    | (Int i)::t -> i + sum t

let three = sum [String "1"; Int 2]

type intlist = Nil | Cons of int * intlist
let lst123 = Cons(1, Cons(2, Cons(3, Nil)))

type t = U of u and u = T of t

type 'a tree = 
    | Leaf
    | Node of 'a * 'a tree * 'a tree

type 'a tree2 = 
    | Leaf2
    | Node2 of 'a node
and 'a node = {
    value: 'a;
    left: 'a tree2;
    right: 'a tree2;
}

let t = 
    Node (4, 
        Node (2,
            Node (1, Leaf, Leaf),
            Node (3, Leaf, Leaf)
        ),
        Node(5,
          Node(6,Leaf,Leaf),
          Node(7,Leaf,Leaf)
        )
    )

let t2 = Node2 {
    value=2;
    left =Node2 {value=1; left=Leaf2; right=Leaf2};
    right = Node2 {value=3; left=Leaf2; right=Leaf2};
}

let rec size = function
    | Leaf -> 0
    | Node (_, l, r) -> 1 + size l + size r

let rec mem x = function
    | Leaf2 -> false
    | Node2 {value; left; right} -> value=x || mem x left || mem x right

let rec preorder = function 
    | Leaf2 -> []
    | Node2 {value; left; right} -> [value] @ preorder left @ preorder right

let preorder_lin t = 
    let rec pre_acc acc = function
        | Leaf2 -> acc
        | Node2 {value;left;right} -> value :: (pre_acc (pre_acc acc right) left) 
    in pre_acc [] t

(* Natural numbers *)
type nat = Zero | Succ of nat
let zero = Zero
let one = Succ zero

let iszero (n: nat) : bool = 
    match n with
    | Zero -> true
    | Succ m -> false

let pred (n: nat) : nat option = 
    match n with
    | Zero -> None
    | Succ m -> Some m

let rec int_of_nat (n: nat) : int = 
    match n with 
    | Zero -> 0
    | Succ m -> 1 + int_of_nat m 

let rec nat_of_int (i: int) : nat = 
    if i < 0 then failwith "nat_of_int is not defined on negative numbers" 
    else if i = 0 then Zero
    else Succ (nat_of_int (i-1))

let rec 
    even (n: nat) : bool = 
        match n with
        | Zero -> true
        | Succ m -> odd m
and 
    odd (n: nat) : bool = 
        match n with
        | Zero -> false
        | Succ m -> even m

