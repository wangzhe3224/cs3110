open Async

let printlen s = 
  printf "%i\n" (String.length s)

let r = Reader.file_contents Sys.argv.(1)

let _ : unit = upon r (fun s -> printlen s; ignore(exit 0 : 'a Deferred.t))

let _ : Core.never_returns  = Scheduler.go()
