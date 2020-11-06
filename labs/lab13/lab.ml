type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec preorder = function
  | Leaf -> []
  | Node (l, v, r) -> [v] @ preorder l @ preorder r

let preorder' t = 
  let rec aux t acc = 
    match t with
    | Leaf -> acc 
    | Node (l, v, r) -> 
      let acc_with_right = aux r acc in
      let acc_with_value = v :: acc_with_right in
      aux l acc_with_value
  in aux t [] 

(* Memory usage of RbSet *)
