(* this is exercise for Chapter 3 *)
(* https://www.cs.cornell.edu/courses/cs3110/2020sp/textbook/data/exercises.html *)
let e11 = [1;2;3;4;5]
let e12 = 1::2::3::4::5::[]
let e13 = [1] @ [2;3;4] @ [5]

let rec product lst = 
    match lst with
    | [] -> 1
    | x::xs -> x * product xs

let rec concat (lst: string list) = 
    match lst with
    | [] -> ""
    | x::xs -> x ^ concat xs

type suit = A | B | C | D
type rank = int
type card = { suit: suit; rank: rank }
let ace_1 = { suit=A; rank=1 }

(* matching *)
let lst = [Some 1; Some 2; None]

