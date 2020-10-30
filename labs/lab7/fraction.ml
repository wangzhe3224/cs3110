module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0.*)
  type t

  (* [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator   : t -> int
  val denominator : t -> int
  val toString    : t -> string
  val toReal      : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module MyFrac : Fraction = struct
    type t = (int*int)
    
    let make a b = (a, b)

    let numerator (a, _) = a

    let denominator (_, b) = b

    let toString (a, b) = string_of_int a ^ "/" ^ (string_of_int b)

    let toReal (a, b) = float_of_int a /. (float_of_int b)

    let add f1 f2 = f1

    let mul f1 f2 = f2
end

