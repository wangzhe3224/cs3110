(* List Monad *)
let fmap ( o : 'a list) (f : 'a -> 'b): 'b list = List.map f o
let join ( o : 'a list list ) = List.concat o
let return x = [x]
let bind (m : 'a list) ( f : 'a -> 'b list ) : 'b list = fmap m f |> join
let (>>=) = bind

let l1 = [1;2;3;4]
let l2 = [1;2;3;4]

let _ = l1 >>= fun x ->
        l2 >>= fun y ->
        return (x + y)
