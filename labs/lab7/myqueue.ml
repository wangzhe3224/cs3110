module ListQueue = struct
    type 'a queue = 'a list

    let empty = []

    let is_empty q = (q=[])

    let enqueue x q = q @ [x]

    let peek = function
        | [] -> failwith "Empty"
        | x::_ -> x

    let dequeue = function 
        | [] -> failwith "Empty"
        | _::q -> q
end

module TwoListQueue = struct
    type 'a queue = {front: 'a list; back: 'a list}

    let empty = {front=[]; back=[]}

    let is_empty = function
        | {front=[]; back=[]} -> true
        | _ -> false

    let norm = function
        | {front=[]; back} -> {front=List.rev back; back=[]}
        | q -> q

    let enqueue x q = norm {q with back=x::q.back}

    let peek = function
        | {front=[]; _ } -> None
        | {front=x::_; _} -> Some x

    let dequeue = function
        | {front=[]; _} -> None
        | {front=_::xs; back} -> Some (norm {front=xs; back})
end

(* Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty

let fill_listqueue' n =
  let rec loop n q =
    if n=0 then q
    else loop (n-1) (TwoListQueue.enqueue n q) in
  loop n TwoListQueue.empty
