open Async

let d1 = Reader.file_contents Sys.argv.(1)
let cb = fun s -> return (String.uppercase_ascii s)
let d2 = Deferred.bind d1 cb
let d3 = Deferred.bind d2 (fun s -> printf "%s\n"s; return ())
let r = upon d2 (fun s -> printf "%s\n" s)

let exit : unit = upon d3 (fun _ -> ignore(exit 0 : 'a Deferred.t))

let rr = Scheduler.go ()
