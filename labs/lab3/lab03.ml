let rec prod lst = 
    match lst with
    | [] -> 1
    | x::xs -> x * prod xs

let rec concat lst = 
    match lst with
    | [] -> ""
    | x::xs -> x ^ concat xs
