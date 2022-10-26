open Cfg.Cfg_intf
open Vector_intf

module Make (Spec : SPEC) : 
  (VECTOR 
    with type letter   := Spec.t 
    and  type alphabet := (int, Spec.t) Hashtbl.t) = struct
module AMap      = Map.Make(struct type t = Spec.t let compare = Spec.compare_t end)


type t    = int    AMap.t
type word = Spec.t list
type pids = t      list

let to_letter g (v : t) : Spec.t = 
  let rec to_letter (v : (Spec.t * int) list) (i : int) = 
    match v with 
    | []   -> failwith "zero vector"
    | (_, 0) :: v -> to_letter v (i + 1)
    | _ :: _ -> Hashtbl.find g i
  in to_letter (AMap.bindings v) 0

let repr (x : word) : t = 
  let rec repr (x : word) (v : t) =
    match x with 
    | []     -> v
    | a :: x -> AMap.update a (Option.map ((+) 1)) v |> repr x
  in repr x AMap.empty

let construct_pids (w : word) : pids = failwith "unimpl"

end