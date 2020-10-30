let rec print_int_list = function 
    | [] -> ()
    | h::t -> print_endline (string_of_int h);
                print_int_list t


let print_int_list' = List.iter (fun x -> print_endline (string_of_int x)) 
