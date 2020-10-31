# Ex.1 Make CharMap 

`# module CharMap = Map.Make(Char);;`, Make is the functor that create a new structue from a structue. 

Explain the type of produced value signiture:
``` ocaml
val empty : 'a t
val add : key -> 'a -> 'a t -> 'a t
val remove : key -> 'a t -> 'a t
```

CharMap is still a Sig, `empty` has type 'a t, t is a genaric type that has one type variable 'a. `key` in this case is a char.

``` ocaml
type key = char
type 'a t = 'a Map.Make(Char).t
```

# Ex2 Char ordered

The `Map.Make` functor requires its input module to match `Map.OrderedType` sig. Why we are allowed to pass
`Char` as an argument to `Map.Make`?

Because the sig of `Char` contains `t` and `compare` val sig.

# Ex3 Use char map

```ocaml
open CharMap;;
let d = let d = empty 
        in let d = add 'A' "Alpha" d in
        add 'E' "Echo" d;;
let b = bindings d;;
(* val b : (key * string) list = [('A', "Alpha"); ('E', "Echo")] *)
```

# Ex4 bindings

```
val bindings : 'a t -> (key * 'a) list
Return the list of all bindings of the given map. The returned list is sorted in increasing order of keys with respect to the ordering Ord.compare, where Ord is the argument given to Map.Make.
Since 3.12.0
```

They all produce the same result. becuase the key is ordered.


# Ex5 date order

Check code in `calendar.ml`

# Writing functors

Code is here: `print.ml`

Explain in your own words how Print has achieved code reuse, albeit a very small amount.

this bit is reused: `let print t = print_endline (M.to_string t)`.

# Compulation units
 
after change to `type date`, the detal of Date is hiden.
``` ocaml
let j1 = Date.make_date 1 1;;
> val j1 : Date.date = <abstr>
```

# Additionals

`let is_for d = CharMap.mapi (fun k v -> (Char.escaped k) ^ v);;`


Print String resue revisit


