open Cfg.Cfg_intf
open Vector_intf

module Make (Spec : SPEC) : (VECTOR with type letter = Spec.t) =
struct
module AMap      = Map.Make(struct type t = Spec.t let compare = Spec.compare_t end)

type alphabet = (int, Spec.t) Hashtbl.t
type t        = int    AMap.t
type word     = Spec.t list
type pids     = t      list
type letter   = Spec.t

let empty : t = AMap.empty

let compare : t -> t -> int = AMap.compare Int.compare
let equal   : t -> t -> bool = AMap.equal   Int.equal

let increment (a : Spec.t) (v : t) : t = 
  AMap.update a (fun x -> Some((Option.value ~default:0 x) + 1)) v

let to_string (v : t) : string = 
  AMap.fold (fun _ n str -> string_of_int n ^ str) v " "

let to_letter g (v : t) : Spec.t = 
  let rec to_letter (v : (Spec.t * int) list) (i : int) = 
    match v with 
    | []   -> failwith "zero vector"
    | (_, 0) :: v -> to_letter v (i + 1)
    | _ :: _ -> Hashtbl.find g i
  in to_letter (AMap.bindings v) 0

let of_word (x : word) : t = 
  let rec repr (x : word) (v : t) =
    match x with 
    | []     -> v
    | a :: x -> AMap.update a (Option.map ((+) 1)) v |> repr x
  in repr x AMap.empty

let delete (a : Spec.t) (v : t) = 
  AMap.update a (Option.map ((-) 1)) v

let union2 : t -> t -> t = AMap.merge (fun _ a b -> 
  let value = Base.Option.value ~default:0 in
  Some(Base.Int.max (value a) (value b)))

type max_vector = First | Second | NotComp

let union (v : t list) : t = 
  match v with 
  | []   -> failwith "emply union"
  | v :: vs -> List.fold_left union2 v vs

let elements (v : t) = AMap.bindings v |> List.map fst

let union_max : t -> t -> t * max_vector = fun v1 v2 ->
  let start      = ref false   in 
  let max_vector = ref NotComp in

  AMap.merge (fun _ a b -> 
    let value = Base.Option.value ~default:0 in
    let a, b = value a, value b in 
    begin match a < b, !start, !max_vector with 
    | c, false, _ -> 
      start      := true; 
      max_vector := if c then Second else First;
    | true, _, First | false, _, Second -> 
      max_vector := NotComp
    | _, _, _ -> ()
    end;
    Base.Option.some @@ Base.Int.max a b
    ) v1 v2,
  !max_vector

  

end