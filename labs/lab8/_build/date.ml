type date = { month:int; day:int }
let make_date m d = {month=m;day=d}
let get_month d = d.month
let get_day d = d.day
let to_string d = (string_of_int d.month) ^ "/" ^ (string_of_int d.day)

