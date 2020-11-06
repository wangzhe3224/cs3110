type ('k, 'v) tree = 
  | Leaf 
  | Node of ('k, 'v) tree * 'k * 'v  * ('k, 'v) tree

module type Dict = sig
  type ('k,'v) t
  val empty : ('k,'v) t
  val mem : 'k -> ('k,'v) t -> bool
  val insert : 'k -> 'v -> ('k,'v) t -> ('k,'v) t
  val find : 'k -> ('k,'v) t -> 'v option
  (* val bindings : ('k,'v) t -> ('k*'v) list *)
  (* val of_list : ('k*'v) list -> ('k,'v) t *)
end

module DictBrTree : Dict = struct
  (* sealed to hide implementation details *)
  type color = Red | Black
  type ('k,'v) t =
    | Leaf 
    | Node of (color * ('k,'v) t * 'k * 'v * ('k,'v) t)

  let empty = Leaf
    
  let rec mem k = function 
    | Leaf -> false
    | Node (_, l, key, value, r) -> 
      if k < key then mem k l
      else if k > key then mem k r
      else true

  let balance = function
    | (Black, Node (Red, Node (Red, a, xk, xv, b), yk, yv, c), zk, zv, d)
    | (Black, Node (Red, a, xk, xv, Node (Red, b, yk, yv, c)), zk, zv, d)
    | (Black, a, xk, xv, Node (Red, b, yk, yv, Node (Red, c, zk, zv, d)))
    | (Black, a, xk, xv, Node (Red, Node (Red, b, yk, yv, c), zk, zv, d))
      -> Node (Red, Node (Black, a, xk, xv, b), yk, yv, Node (Black, c, zk, zv, d))
    | t -> Node t

  let rec insert' k v = function
    | Leaf -> Node (Red, Leaf, k, v, Leaf)
    | Node (col, l, kk, vv, r) -> 
      if k > kk then balance (col, l, kk, vv, ( insert' k v r ))
      else if k < kk then balance (col, ( insert' k v l ), kk, vv, r)
      else Node (col, l, k, v, r)

  let insert k v d = 
    match insert' k v d with
    | Node (_, l, kk, vv, r) -> Node (Black, l, kk, vv, r)  (* make root black ? *)
    | Leaf -> failwith "impossible!"

  let rec find k = function
    | Leaf -> None
    | Node (_, l, kk, vv, r) -> 
      if k < kk then find k l
      else if k > kk then find k r
      else Some vv

end

let d = DictBrTree.empty
let d2 = DictBrTree.(empty |> (insert 1 1) |> (insert 2 2) |> (insert 3 3))
