open Async
    
let output s = printf "%s\n%!" s

(* let d1 = return 42 *)
let d1 = after (Core.sec 5.) 
let d2 = return 42
let d3 = return 42

let a = output "A"
let b = upon d2 (fun x -> upon d3 (fun y -> output "B"))
let c = upon d1 (fun x -> output "C")
let d = output "D"

let r = Scheduler.go()
