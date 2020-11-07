module ArrayMap = struct
  (* AF: [|Some v0; Some v1; ...|] represents {0:v0, 1:v1, ...}
   * But if element [i] of [a] is None, then [i] is not bound in the map*)
  type 'v t = 'v option array

  let create n = Array.make n None

  let insert k v a = a.(k) <- Some v

  let find k a = a.(k)

  let remove k a = a.(k) <- None
end

type ('k, 'v) t = {
  hash : 'k -> int;
  mutable size : int;
  mutable buckets : ('k*'v) list array
}

(* AF: If [buckets] is 
 *    [|[(k11, v11); (k12, v12); ...];
 *      [(k21, v21); (k22, v22); ...]; ...|]
 * that represents the map 
 *    {k11: v11, k12: v12, k22:v22, k21: v21}
 * RI: No key appears more than once in array. 
 *    All keys are in the right buckets: 
 *    [k] appears in [buckets] at index b iff [hash(k)=b]
 *    Number of binding is equals [size]. *)
