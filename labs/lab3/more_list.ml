let first = function
    | "bigred"::xs -> true
    | _ -> false

(* pattern match failure *)
let first' ("bigred"::xs) = true

let second = function
    | [_;_] -> true
    | [_;_;_;_] -> true
    | _ -> false

let third = function
    | a::b::xs -> a = b
    | _ -> false

(* Library: list *)
let fifth lst = 
    let length = List.length lst in
        if length < 5 then 0
        else List.nth lst 4

let last_ele lst = 
    let length = List.length lst in
        List.nth lst (length - 1)

let any_zeros = List.mem 0 

let rec take_slow k xs = 
    (* here is not a tail rec *)
    match xs with
    | [] -> []
    | y::ys -> match k with 
                | 0 -> y::ys
                | k -> y::(take (k-1) ys)  

let take k xs = 
    let rec take_help acc k' xs' = 
        match xs' with
        | [] -> acc
        | y::ys -> match k' with 
                    | 0 -> acc
                    | k' -> take_help (y::acc) (k'-1) ys (*ok, in this func k and k' matters*)
                    (* maybe it is better just shadow the args, like using just k xs *)
    in 
    take_help [] k xs |> List.rev  (* again ... this looks bad.. *)

let take' k xs = 
    let rec take_help acc k xs = 
        match xs with
        | [] -> acc
        | y::ys -> match k with 
                    | 0 -> acc
                    | k -> take_help (y::acc) (k-1) ys (*ok, in this func k and k' matters*)
                    (* maybe it is better just shadow the args, like using just k xs *)
    in 
    take_help [] k xs |> List.rev  (* again ... this looks bad.. *)

let rec drop k xs = 
    (* this is a tail rec *)
    match xs with
    | [] -> []
    | y::ys -> match k with
                | 0 -> y::ys
                | k -> drop (k-1) ys

(* returns: [from i j l] is the list containing integer from 
 *  [i] to [j], inclusive, followed by the list [l].
 * example: [from 1 2 [0] = [1;2;3;0]]
 *)
let rec from i j l = 
    if i > j then l
    else from i (j-1) (j::l)

let (--) i j = from i j []

let longlist = 0 -- 1_000_000
