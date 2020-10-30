module type Dictionary = sig
    type ('k, 'v) t

    (* The empty dict *)
    val empty : ('k, 'v) t

    val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

    val lookup  : 'k -> ('k,'v) t -> 'v
end

module AssocListDict : Dictionary = struct
(* Using sig will hide the underly type details of this module *)
(* utop # let d = insert 1 "one" empty;; *)
(* val d : (int, string) t = <abstr> *)
(* vs *)
(* utop # d;; *)
(* - : (int * string) list = [(1, "one")] *)
  type ('k, 'v) t = ('k * 'v) list

  let empty = []

  let insert k v d = (k, v)::d

  let lookup k d = List.assoc k d

end
