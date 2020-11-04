(* 1. Streams *)
type 'a stream =
  Cons of 'a * (unit -> 'a stream)

let pow2 = let rec _pow2 n = Cons (2.**n, fun () -> _pow2 (n+.1.)) 
           in _pow2 0.

let rec from n = Cons (n, fun () -> from (n+1))
let rec fromf n = Cons (n, fun () -> fromf (n+.1.))

let nats = from 0;;
let natsf = fromf 0.;;

(* [hd s] is the head  of [s] *)
let hd (Cons (h, _)) = h

(* [tl s] is the tail of [s] *)
let tl (Cons (_, tf)) = tf ()

let rec sum (Cons (h1, tl1)) (Cons (h2, tl2)) = 
    Cons (h1+.h2, fun () -> sum (tl1 ()) (tl2 ()))

let rec fibs = 
    Cons (1., fun () -> 
        Cons (1., fun () -> 
            sum fibs (tl fibs)))

let rec total' = function
    | Cons (p, g) -> sum (Cons (p, g)) (tl (total' (g())))

let rec map f (Cons (h, tf)) = 
    Cons (f h, fun () -> map f (tf ()))

let rec map2 f (Cons (h1, tf1)) (Cons (h2, tf2)) = 
    Cons (f h1 h2, fun () -> map2 f (tf1 ()) (tf2 ()))

let sum' = map2 (+)

let rec take n s = 
    if n=0 then []
    else hd s :: take (n-1) (tl s)

let rec drop n s = 
    if n=0 then s
    else drop (n-1) (tl s)

(* let nth stream idx *) 
let nth s i = take i s |> List.rev |> List.hd 

let rec filter p s = 
    if p (hd s) then Cons (hd s, fun () -> filter p (tl s))
    else filter p (tl s)

let even = filter (fun n -> n mod 2 = 0) nats
let even10 = take 10 even
(* val even10 : int list = [0; 2; 4; 6; 8; 10; 12; 14; 16; 18] *)

let rec interleave s1 s2 = 
    match s1, s2 with
    | Cons (h1, tl), s2 -> Cons (h1, fun () -> interleave s2 (tl ()))

let inter = interleave pow2 natsf

let sift n s = filter (fun x -> x mod n <> 0) s

let prime = 
    let rec sieve = function
        | Cons (p, g) -> 
                let next = sift p (g ()) in
                Cons (p, fun () -> sieve next)
    in sieve (from 2) 

let rec constant n = Cons (n, fun () -> constant n)

let rec facteus n = 
    if n=0.0 then 1.0
    else if n=1.0 then 1.0
    else n *. facteus (n-.1.)

let e_terms x = 
    let rec term (Cons (p, g)) x = 
        Cons (x**p /. (facteus p), fun () -> term (g()) x)
    in term (fromf 0.) x

let rec total (Cons (p, g)) = Cons (p, fun () -> total (Cons (p+.(hd (total (g()))), g)))

let sum_up_to_n : float stream -> int -> float = 
    let rec loop acc s n = 
        if n<=0 then acc
        else loop (acc +. hd s) (tl s) (n-1) in
    loop 0.0

let rec within eps (Cons (p, g)) = 
    let diff = abs_float (p -. hd (g())) in
    if diff < eps then p
    else within eps (g())

let test = Cons (1., fun () -> Cons (6.1, fun () -> constant 7.))

let e x eps = within eps (total (e_terms x))

(* Laziness *)
let fib30lazy = lazy (take 30 fibs |> List.rev |> List.hd)
(* let fib30 = Lazy.force fib30lazy *)

module StreamFibs = struct
    type 'a stream = 
        | Cons of 'a * (unit -> 'a stream)

    let hd : 'a stream -> 'a =
        fun (Cons (h, _)) -> h

    let tl : 'a stream -> 'a stream =
        fun (Cons (_, t)) -> t ()

    let rec take_aux n (Cons (h, t)) lst = 
        if n=0 then lst
        else take_aux (n-1) (t ()) (h::lst)

    let take : int -> 'a stream -> 'a list = 
        fun n s -> List.rev (take_aux n s [])

    let nth : int -> 'a stream -> 'a = 
        fun n s -> List.hd (take_aux (n+1) s [])

    let rec sum : int stream -> int stream -> int stream = 
        fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
          Cons (h_a + h_b, fun () -> sum (t_a ()) (t_b ()))

    let rec fibs = 
      Cons(1, fun () -> 
          Cons(1, fun () -> 
              sum (tl fibs) fibs))

    let nth_fib n =
      nth n fibs
end

module LazyFibs = struct
  type 'a lazystream = 
    | Cons of 'a * 'a lazystream Lazy.t

  let hd : 'a lazystream -> 'a = 
    fun (Cons (h, t)) -> h 

  let tl : 'a lazystream -> 'a lazystream = 
    fun (Cons (_, t)) -> Lazy.force t

  let rec take_aux n (Cons (h, t)) lst = 
    if n = 0 then lst 
    else take_aux (n-1) (Lazy.force t) (h::lst) 

  let take : int -> 'a lazystream -> 'a list =
    fun n s -> List.rev (take_aux n s [])

  let nth : int -> 'a lazystream -> 'a =
    fun n s -> List.hd (take_aux (n+1) s [])

  let rec sum : int lazystream -> int lazystream -> int lazystream =
    fun (Cons (h_a, t_a)) (Cons (h_b, t_b)) ->
      Cons (h_a + h_b, lazy (sum (Lazy.force t_a) (Lazy.force t_b)))

  let rec fibs = 
    Cons(1, lazy (
        Cons(1, lazy (
            sum (tl fibs) fibs))))

  let nth_fib n =
    nth n fibs

end

let lazy_hello : unit Lazy.t = lazy (print_endline "Hello lazy world.")

let (&&&) : bool Lazy.t -> bool Lazy.t -> bool = 
    fun b1 b2 -> 
        let res1 = Lazy.force b1 in
        if res1 then res1 && (Lazy.force b2)
        else false

module LazyList = struct
    type 'a lazylist = 
        Cons  of 'a * 'a lazylist Lazy.t

    let hd : 'a lazylist -> 'a = 
        fun ( Cons (h, _) ) -> h

    let tail : 'a lazylist -> 'a lazylist = 
        fun ( Cons (_, t) ) -> Lazy.force t

    let rec take' : int -> 'a lazylist -> 'a list = 
        fun n (Cons (h, t)) -> 
            if n=0 then []
            else h::(take' (n-1) (( Lazy.force t )))

    let rec froml : int -> int lazylist =
        fun n -> Cons (n, lazy ( froml (n+1) ))

    let rec fib = Cons (1,
                    Cons 1, 
                        )
end

let a = LazyList.froml 100
