(* let bind (x : int option) (op : int -> int option) : int option = *)
(*   match x with *)
(*   | None -> None *)
(*   | Some a -> op a *)

(* let (>>=) = bind *)

(* let return (x : int) : int option = *)
(*   Some x *)

(* let add (x: int option) (y: int option) = *)
(*   x >>= fun x' -> *) 
(*   y >>= fun y' -> *) 
(*   return (x'+y') *)

module type Dummy = sig
  val foo : unit -> int option
  val bar : int -> string option
  val baz : unit -> string option
  val print_option : string option -> unit
end
(* Maybe Monad *)
let return x = Some x 
let bind m f = 
  match m with 
    | Some x -> f x 
    | None -> None
let (>>=) = bind
let fmap ( o : 'a option) (f : 'a -> 'b): 'b option = 
  match o with 
  | None -> None
  | Some x -> Some (f x)
let join : ('a option option -> 'a option) = function
  | None -> None
  | x -> match x with
    | None -> None
    | Some x -> x
let bind2 ( m : 'a option ) ( f : 'a -> 'b option) : 'b option = fmap m f |> join
let fmap2 m f = bind m (fun m' -> return (f m')) 
let join n = bind n (fun n' -> n')

let foo () = Some 1
let bar n = Some "bar"
let baz () = Some "baz"
let print_option = function 
  | None -> ()
  | Some s -> print_endline s

(* bind   : 'a t -> ('a -> 'b t) -> 'b t *)
(* return : 'a -> 'a t*)
(* fmap   : 'a t -> 'a -> 'b -> 'b t 
 * join   : 'a t t -> 'a t*)
let a = foo () >>= fun x -> 
        bar x >>= fun y ->
        baz () >>= fun z -> 
        print_option (Some (y^z)); None

(* fmap : 'a t -> 'a -> 'b -> 'b t*)
(* fmap' : 'a -> 'b -> 'a t -> 'b t
 * this is some time called lift *)

