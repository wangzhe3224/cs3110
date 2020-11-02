(* [Poly] represents immutable polynomials with integer cofficient *)
module type Poly = 
    sig
        (* [t] is the type of polynomials *)
        type t

        (* [eval x p] is [p] evaluated at [x]. 
         * example: if [p] represents $3x^3+x^2+x$, then
         * [eval 10 p] is [3110] *)
        val eval : int -> t -> int
        (* [single coff order] is one component, coff*x^order *)
        val single : int -> int -> t
        (* [combine t1 t1] creates polynomials as t1 + t2 *) 
        val combine : t -> t -> t
        (* [to_string t] creates the string reprensation of the the polynomials *)
        val to_string : t -> string
    end
