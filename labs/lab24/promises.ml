(** A signature for Lwt-style promises, with better names *)
module type Promise = sig
  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise  (* client interface *)
  type 'a resolver (* internal *)
  type 'a handler

  (** [make ()] is a new promise and resolver. The promise is Pending. *)
  val make : unit -> 'a promise * 'a resolver

  (** [return x] is a new promise that is already resolved with value [x]. *)
  val return : 'a -> 'a promise

  (** [state p] is the state of the promise. *)
  val state : 'a promise -> 'a state

  (** [resolve r x] resolves the promise [p] associated with [r]
   *  with value [x], meaning that [state p] will become [Resolved x]. 
   *  Requires: [p] is pending. *)
  val resolve : 'a resolver -> 'a -> unit

  (** [reject r x] rejects the promise [p] associated with [r]
      with exception [x], meaning that [state p] will become
      [Rejected x].
      Requires:  [p] is pending. *)
  val reject : 'a resolver -> exn -> unit

  (** [p >>= c] regiesters callback [c] with promise [p].
   * When the promise is resolved, the callback will be run
   * on the promises's contents. If promise is never 
   * resolved, the callback will never run. *)
  val (>>=) : 'a promise -> ('a -> 'b promise) -> 'b promise
end

module TwlPromise : Promise = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list
  } 
  type 'a resolver = 'a promise
  type 'a handler = 'a state -> unit

  let write_once p s = 
    if p.state = Pending
    then p.state <- s
    else invalid_arg "cannot write twice"

  let enqueue 
      (handler : 'a state -> unit) 
      (promise : 'a promise) : unit 
    =
    promise.handlers <- handler :: promise.handlers

  let make () = 
    let p = {state=Pending; handlers=[]} in
    p, p;

  let return x ={state=Resolved x; handlers=[]}

  let state p = p.state 

  (** requires: [st] may not be [Pending] *)
  let resolve_or_reject (r : 'a resolver) (st : 'a state) = 
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let reject r x = 
    resolve_or_reject r (Rejected x)

  let resolve r x =  
    resolve_or_reject r (Resolved x)

 let handler (resolver : 'a resolver) : 'a handler
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x -> resolve resolver x

  let handler_of_callback 
      (callback : 'a -> 'b promise) 
      (resolver : 'b resolver) : 'a handler 
    = function
      | Pending -> failwith "handler RI violated"
      | Rejected exc -> reject resolver exc
      | Resolved x ->
        let promise = callback x in
        match promise.state with
        | Resolved y -> resolve resolver y
        | Rejected exc -> reject resolver exc
        | Pending -> enqueue (handler resolver) promise      

  let (>>=) 
      (input_promise : 'a promise) 
      (callback : 'a -> 'b promise) : 'b promise 
    = 
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> {state = Rejected exc; handlers = []}
    | Pending -> 
      let output_promise, output_resolver = make () in
      enqueue (handler_of_callback callback output_resolver) input_promise;
      output_promise let (>>=) 
      (input_promise : 'a promise) 
      (callback : 'a -> 'b promise) : 'b promise 
    = 
    match input_promise.state with
    | Resolved x -> callback xet (>>=) p c = 
end
