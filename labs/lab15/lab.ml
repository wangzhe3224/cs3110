(* Hashing *)
(* Hash insert *)
let hash k = k mod 7
let keys = [4;8;15;16;23;42]
let idx = List.map hash keys
(* val idx : int list = [4; 1; 1; 2; 2; 0] *)

(* relaxt bucket RI: allow duplicates.*)
(* Hashtbl *)
(* [i -- j] returns ints from i to j
 * Requires i <= j. *)
let (--) i j = 
  let rec aux n acc = 
    if n<i then acc
    else aux (n-1) (n::acc)
  in aux j [];;

let tab = Hashtbl.create 16;;
List.map (fun x -> Hashtbl.add tab x x) (1--31) 

let bindings h = let func k d acc =
                   (k, d)::acc
  in Hashtbl.fold func h []
;;

let tab_bindings = bindings tab;;

let load_factor ht = let stats = Hashtbl.stats ht in
  stats.num_bindings / stats.num_buckets
(* Hashtable's load factor always below 2.0 *)

(* Functorial Hashtbl *)
module StrHash = struct
  type t = string
  let equal i j = i = j
  let hash i = Hashtbl.hash i
end

module StrHashTable = Hashtbl.Make(StrHash)

let strtab = StrHashTable.create 32;;
StrHashTable.add strtab "S" "S";
StrHashTable.add strtab "s" "s";

