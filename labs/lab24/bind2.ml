open Async

let r = Reader.file_contents Sys.argv.(1) >>= fun s ->
  printf "%s\n" (String.uppercase_ascii s); exit 0;;

let rr = Scheduler.go ()
