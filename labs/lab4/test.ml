(* Data Types *)
(* Every data type is releated to a Pattern, which can be matched. *)
(* Variant *)
type ptype = TNormal | TFire | TWater

(* Records *)
type mon = {name: string; hp: int; ptype: ptype}
let mon1 = {name = "Ch"; hp = 30; ptype=TFire}
let matched c = (* get hp *) 
    match c with 
    | {name; hp; ptype} -> hp
let copied = {mon1 with hp=20; ptype=TNormal}

(* Tuples *)
let t1 = (1,2,3)
let t2 = (1, "hello")
let p1 = match (1,2,3) with 
            | (x,y,z) -> y

(* Pattern matching every where 
 let p = e1 in e1 is actally a pattern matching special case
 function def is pattern matching as well..
 *)
let x::xs = [1;2;3]  (* x is bind to 1, xs is bind to [2;3]. But pattern matching is exhaustive *)

let get_hp m = 
    match m with
    | {hp} -> hp
let get_hp' m = m.hp

let thrd (_, _, z) = z

(*
  or pattern: p1 | p2 | ... | pn
  pattern with type annotation: (p: t)
 *)
