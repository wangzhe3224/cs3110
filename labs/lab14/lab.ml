let inc = ref (fun x -> x+1)
let cs3110 = !inc 3109

(* addition assigment *)
let (+:=) (x: int ref) ( y: int ) : unit = 
  x := !x + y

(* physical equality, e1 == e2, if and only if modification 
 * of e1 also affect e2 
 * structural equality, will compare the logical value *)
       
(* Array *)
(* AF: the float array [| x1; ...; xn|] represents the
 *    vector (x1, ..., xn)
 * RI: the array is NOT empty *) 
type vector = float array

let norm v: float = Array.map (fun x -> x*.x) v |> Array.fold_left (+.) 0. |> sqrt

let normalize v : unit = let n = norm v in Array.iteri (fun i _val -> v.(i) <- _val/.n) v

let v = [| 1.; 1.|]

(* Loops *)
let normalize_loop v = 
  let l = Array.length v in
  let n = norm v in 
  let counter = ref 0 in
  while !counter < l do
    v.(!counter) <- v.(!counter) /. n;
    counter +:= 1
  done

let fact (n: int) = 
  let res = ref 1 in
  for x=1 to n do
    res := !res * x 
  done;
  !res

(* Double linked list *)
(* An ['a node] is a node of a mutable double linked
 * list. It contains a value of type ['a] and optionally
 * has poiners to previous and/or next nodes. *)
type 'a node = {
  mutable prev: 'a node option;
  mutable next: 'a node option;
  value: 'a
}

(* An ['a dlist] is a mutable doubly-lined list 
 * with elements of type 'a. It is possible to 
 * access the first and last element in constant
 * time.
 * RI: the list does not have cycles *)
type 'a dlist = {
  mutable first : 'a node option;
  mutable last  : 'a node option;
}

(* [create_node v] is a node containing value [v]
 * with no links to other nodes. *)
let create_node v = {prev=None; next=None; value=v}

(* [empty_dlist ()] is an empty double linked list. *)
let empty_dlist () = {first=None; last=None}

let create_dlist (n: 'a node) : 'a dlist = {first=Some n; last=Some n}

(* [insert_dlist d n] mutates dlist [d] by 
 * inserting node [n] as the first node. *)
let insert_first (d: 'a dlist) (n: 'a node) : unit =
  let first = d.first in
  begin match first with
    | None -> ()
    | Some nd -> nd.prev <- Some n
  end;
  n.next <- d.first;
  n.prev <- None;
  d.first <- Some n;;

(* [insert_last d n] mutates dlist [d] by
* inserting node [n] as the last node. *)
let insert_last (d: 'a dlist) (n: 'a node) : unit =
  let last = d.last in
  begin match last with
    | None -> ()
    | Some nd -> nd.next <- Some n
  end;
  n.prev <- d.last;
  n.next <- None;
  d.last <- Some n;;

(* [insert_after d n1 n2] mutates dlist [d] by
 * inserting node [n2] after node [n1]. *)
let insert_after d (n1: 'a node) ( n2 : 'a node ) : unit = 
  if d.last == Some n1 then insert_last d n2
  else let next_n1 = n1.next in
    n2.prev <- Some n1;
    n2.next <- next_n1;;

(* [iter_forward d f] on a dlist [d] which has 
 * elements n1; n2;... is (f n1); (f n2) ... *)
let iter_forward (d: 'a dlist) (f: 'a -> unit) : unit = 
  let next_ref = ref d.first in 
  let apply f = function
    | None -> ()
    | Some nd -> f nd.value 
  in
  while !next_ref <> None do
    let next = !next_ref in 
    apply f next;
    match next with 
    | None -> ()
    | Some nd -> next_ref := nd.next
  done;;

let lst = empty_dlist ();;
insert_last lst (create_node 1);
insert_last lst (create_node 2);

type 'a dlink = {
  mutable data: 'a;
  mutable next: 'a dlink option;
  mutable prev: 'a dlink option;
}

let dlink_of_list li =
  let f prev_dlink x =
    let dlink = {
      data = x;
      prev = None;
      next = prev_dlink }
    in
    begin match prev_dlink with
    | None -> ()
    | Some prev_dlink ->
        prev_dlink.prev <- Some dlink
    end;
    Some dlink
  in
  List.fold_left f None (List.rev li)
;;
 
let list_of_dlink =
  let rec aux acc = function
  | None -> List.rev acc
  | Some{ data = d;
          prev = _;
          next = next } -> aux (d::acc) next
  in
  aux []
;;
 
let iter_forward_dlink f =
  let rec aux = function
  | None -> ()
  | Some{ data = d;
          prev = _;
          next = next } -> f d; aux next
  in
  aux
;;

let dl = dlink_of_list [1;2;3;4;5] 
