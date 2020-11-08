open Ast

(* [is_value e] is whether [e] is a syntactic value *)
let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Add _ | Var _ | Let _ | Mult _ | If _ | Le _ -> false

(* [subst e v x] is e{v/x}, that is, [e] with [v]
 * substituted for [x]. *)
let rec subst e v x = match e with
  | Var y      -> if x=y then v else e
  | Int n      -> e 
  | Bool b     -> e 
  | Add(el,er) -> Add(subst el v x, subst er v x)
  | Let(y,ebind,ebody) ->
      let ebind' = subst ebind v x in
      if x=y
      then Let(y, ebind', ebody)
      else Let(y, ebind', subst ebody v x)
  | Mult(el, er) -> Mult(subst el v x, subst er v x)
  | Le(el, er) -> Le(subst el v x, subst er v x) 
  | If(e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)

(* A single step of evaluation: exactly 1 step *)
let rec step : expr -> expr = function
  | Int n               -> failwith "Does not step"
  | Var _               -> failwith "Unbound variable"
  | Add(Int n1, Int n2) -> Int (n1 + n2)
  | Add(Int n1, e2)     -> Add (Int n1, step e2)
  | Add(e1, e2)         -> Add (step e1, e2)
  | Let(x, Int n, e2)   -> subst e2 (Int n) x
  | Let(x, e1, e2)      -> Let (x, step e1, e2)
  | Mult(Int n1, Int n2)-> Int (n1*n2)
  | Mult(Int n1, e2)    -> Mult (Int n1, step e2)
  | Mult(e1, Int n2)    -> Mult (step e1, Int n2)
  | Le(Int n1, Int n2)  -> Bool (n1 <= n2) 
  | Le(e1, Int n2)      -> Le (step e1, Int n2)
  | Le(Int n1, e2)      -> Le (Int n1, step e2)
  | If(Bool true, e1, _)-> e1
  | If(Bool false, _, e2)-> e2
  | If(Int _, _,_)      -> failwith "int cannot be used as bool."
  | If(e1, e2, e3)      -> If(step e1, step e2, step e3)

(* [eval e] is the [e -->* v] judgement.  That is,
 * keep applying [step] until a value is produced.  *)
let rec eval : expr -> expr = fun e ->
  if is_value e then e else eval (step e)

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Extract a value from an ast node.
   Raises Failure if the argument is not a node containing a value. *)
let extract_value = function
  | Int i -> VInt i
  | Bool b -> VBool b
  | _ -> failwith "Not a value"

(* Interpret an expression *)
let interp (e:string) : value =
  e |> parse |> eval |> extract_value

