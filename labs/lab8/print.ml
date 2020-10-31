module type ToString = 
    sig
        type t
        val to_string : t -> string
    end

module Print (M: ToString) = 
    struct
        let print t = print_endline (M.to_string t)
    end

module Int = struct
    type t = int
    let to_string = string_of_int
end

module MyString = struct
    type t = string
    let to_string t = t 
    end

module StringWithPrint = struct
    include string
    type t = string
    let to_string t = t 
end

module PrintInt = Print(Int)
module PrintString = Print(MyString)
(* module PrintWith = Print(StringWithPrint) *)
