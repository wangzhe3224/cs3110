type student = { first_name : string ; last_name : string ; gpa : float }
type poketype = Normal | Fire | Water
type pokemon = { name : string ; hp : int ; ptype : poketype }

let charizard = { name="charizard"; hp=78 ; ptype=Fire }
let metapod = { name="metapod"; hp=50 ; ptype=Normal }

let rec list_max = function
    | [] -> None
    | h::t -> begin
        match list_max t with
          | None -> Some h
          | Some m -> Some (max h m)
    end

let safe_hd : 'a list -> 'a option = function
    | [] -> None
    | h::_ -> Some h

let safe_tl = function
    | [] -> None
    | x::xs -> Some xs 

(** [max_hp lst] is the element with max hp of [lst] *)
let max_hp (lst: pokemon list) = function
    | [] -> None
    | lst -> let order (a: pokemon) (b: pokemon) = 
        if a.hp > b.hp then -1
        else if a.hp = b.hp then 0
        else 1 in
              let sorted = List.fast_sort order lst 
              in safe_hd sorted

let m1 = { name="m1"; hp=1; ptype=Normal }
let m2 = { name="m2"; hp=2; ptype=Fire }
let list_ms = [m1; m2]

(* Tuple *)
type date = (int*int*int)
let is_before (a: date) (b: date) = 1

let d = [("rec", 4), ("tri", 3)] 
let insert k v d = (k, v)::d
let rec lookup k = function
    | [] -> None
    | (k', v)::t -> if k=k' then Some v else lookup k t
