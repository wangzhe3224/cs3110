module type Set = sig
  type 'a t
  val empty  : 'a t
  val insert : 'a -> 'a t -> 'a t
  val mem    : 'a -> 'a t -> bool
end

module ListSet : Set = struct
  (* AF: [x1; ...; xn] represents
   *    the set {x1, ..., xn}
   * RI: no duplicates. *)
  type 'a t = 'a list
  let empty = []
  let mem = List.mem
  let insert x s = 
    if mem x s then s else x::s 
end

module BstSet = struct
  (* AF: [Leaf] represents the empty set
   *    [Node (l, v, r)] represents
   *  the set $AF(l) \union {v} \union AF(r)$
   * RI: for every [Node (l, v, r)], 
   *    all the values in [l] are strictly less than
   *    [v], and all the values in [r] are strictly 
   *    larger than [v]. *)
  type 'a t = 
    | Leaf 
    | Node of 'a t * 'a * 'a t

  let rec mem x = function 
    | Leaf -> false
    | Node (l, v, r) ->
      if x < v then mem x l
      else if x > v then mem x r
      else true

  let rec insert x = function 
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) -> 
      if x < v then Node (insert x l, v, r)
      else if x > v then Node (l, v, insert x r)
      else Node (l, x, r)
end

module RbSet = struct
  (* AF: [Leaf] represents the empty set
   *     [Node (c, l, v, r)] represents 
   *     the set AF(l) union v union AF(r).
   * RI: + [Node (Red, _, _, _)] has no Red child 
   *     Every path from the root to a leaf has the 
   *     same number of black nodes.
   *     This implies that length of the longest path
   *     is at most twice length of the shortest path.
   * root node is black *) 
  type color = Red | Black

  type 'a t = 
    | Leaf 
    | Node of (color * 'a t * 'a * 'a t)

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (_, l, v, r) -> 
      if x < v then mem x l
      else if x > v then mem x r
      else true

  let balance = function
    | (Black, Node (Red, Node (Red, a, x, b), y, c), z, d) (* 1 *) 
    | (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d) (* 2 *)
    | (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) (* 3 *)
    | (Black, a, x, Node (Red, Node (Red, b, y, c), z, d)) (* 4 *)
      -> Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
    | t -> Node t

  let rec insert' x = function 
    | Leaf -> Node (Red, Leaf, x, Leaf)
    | Node (col, l, v, r) -> 
      if x < v then balance (col, (insert' x l), v, r)
      else if x > v then balance (col, l, v, insert' x r)
      else Node (col, l, x, r)

  let insert x s = 
    match insert' x s with
    | Node (_, l, v, r) -> Node (Black, l, v, r)
    | Leaf -> failwith "impossible!"
end

(* Some test values *)
let test = RbSet.(empty |> insert 1 |> insert 2 |> insert 100 |> insert 3)

let range n = List.init n succ

let test2 = List.fold_left (fun a x -> RbSet.insert x a) RbSet.empty (range 1000000)
