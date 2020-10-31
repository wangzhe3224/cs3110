type date = { month:int; day:int }

module Date = 
    struct
        type t = date
        let compare d1 d2 = 1
    end

module DateMap = Map.Make(Date);;
type calendar = string DateMap.t

let c1 = DateMap.(empty |> add { month=2; day=1 } "something" |> add {month=2; day=3} "another thing..")

let string_of_date {month; day} = string_of_int month ^ "-" ^ string_of_int day
let print_calendar = DateMap.iter (fun k v -> print_endline (string_of_date k ^ ": " ^ v))
