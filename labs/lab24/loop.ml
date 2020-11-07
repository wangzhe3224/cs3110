open Async

let stdin : Reader.t = Lazy.force Reader.stdin

let shout () : unit Deferred.t = 
  (* prompt the user using [printf] *)
  printf "> ";
  (* read the input using [Reader.read_line stdin] *)
  Reader.read_line stdin >>= fun r -> 
  (* wait 3 seconds using [after] *)
  after (Core.sec 3.0) >>= fun _ -> 
  (* print out whatever the user entered, but converted to all caps, using [printf];
     or if EOF was reached, print nothing *)
  match r with 
  | `Eof -> exit 0 
  | `OK  -> printf "%s\n" line
;; (* exit the program using [exit] *)

let r = shout ()
let s = Scheduler.go ()
