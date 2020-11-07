(* ref is an immutable pointer to a 
 * typed memory location. *)
let next_val = 
  let counter = ref 0
  in fun () -> counter := (!counter) + 1;
    !counter

(* ref is signle mutable value, while 
 * Arrays are generalized ref. *)
let v = [|0.1; 0.2|]
