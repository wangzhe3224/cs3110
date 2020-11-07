open Lwt_io

let p = 
  Lwt.bind (read_line stdin) (fun s1 -> 
    Lwt.bind (read_line stdin) (fun s2 -> 
      Lwt_io.printf "%s\n" (s1^s2)))

let _ : unit = Lwt_main.run p

