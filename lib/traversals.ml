open Cfg

module type ACC  = sig

  type elt
  type t

  val add         : elt -> t -> t
  (* val merge          : t -> t -> t *)
  val compare_elt : elt -> elt -> int
  val singleton   : elt -> t

  val mem         : elt -> t -> bool
  val empty       : t

  val pp_elt      : elt -> string
  val pp_t        : t -> string
  val exists      : (elt -> bool) -> t -> bool
  val union       : t -> t -> t
  
end

module type Show = sig
  type t
  val pp : t -> string
end

module Make (Spec : Ideals_intf.SPEC) (Acc : ACC with type elt = Spec.letter) (Show : Show) = struct

  module LMap   = Map.Make(struct type t = Spec.letter let compare = Acc.compare_elt end)

  type acc_set = Acc.t LMap.t
  type word    = Spec.letter list

  let upd_acc_dep (c : Spec.conc_rel) (base : Spec.letter) : Acc.t -> Acc.t = 
    fun accs -> 
      if Acc.exists (fun x -> not @@ c base x) accs then 
        Acc.add base accs
      else accs

  let pp_acc_set (accs : acc_set) : string = 
    LMap.fold (fun l acc str -> 
      let pp_l   = Spec.pp_letter l ^ ":" in
      let os     = LMap.find_opt l accs   in
        match os with 
        | None   -> ""
        | Some s -> pp_l ^ Acc.pp_t acc ^ "\n" ^ str) accs ""

  let traverse ?verbose
    (w       : word) 
    (c       : Spec.conc_rel)
    (upd_ans : Spec.letter -> acc_set -> Show.t -> Show.t)
    (def     : Show.t)  : Show.t = 
    let rec traverse (x : Show.t) (w : word) (accs : acc_set) = 
      match w with 
      | []     -> x
      | e :: w -> 
        (if verbose = Some true then Printf.printf "%s%!\n" @@ Show.pp x);
        (if verbose = Some true then Printf.printf "%s%!\n" @@ pp_acc_set accs);
        let x    = upd_ans e accs x in
        let accs = 
          LMap.map (upd_acc_dep c e) accs |>
          LMap.add e (Acc.singleton e) 
        in traverse x w accs
      in traverse def w LMap.empty

  let upd_dep
    (c : Spec.conc_rel) 
    (base : Spec.letter) : Spec.letter -> Acc.t -> Acc.t -> Acc.t = 
    fun l accs -> 
      if not @@ c base l then 
        Acc.union accs
      else Base.Fn.id


  let traverse_bw ?verbose
    (w       : word)
    (c       : Spec.conc_rel)
    (upd_ans : Spec.letter -> Acc.t -> Show.t -> Show.t)
    (def : Show.t) : Show.t = 
    let rec traverse (x : Show.t) (w : word) (accs : acc_set) =
      match w with 
      | []     -> x
      | e :: w -> 
        let acc  = LMap.fold (upd_dep c e) accs Acc.empty |> Acc.add e in
        let accs = LMap.add e acc accs                                 in
        let x    = upd_ans e acc x                                     in
          traverse x w accs
    in traverse def w LMap.empty
end

module AfterSet : (ACC with type elt = string) = struct

  include Set.Make(String)
  let compare_elt : elt -> elt -> int = Base.String.compare
  let pp_elt      = Base.String.of_string

  let pp_elt_ws e = pp_elt e ^ " "
  let pp_t        = fun s -> Base.String.concat @@ List.map pp_elt_ws @@ elements s
  
end

module VectorClock_Make (SpecG : Cfg.Cfg_intf.SPEC) : 
  (ACC with type elt = SpecG.t
       and  type t = Vector.Make (SpecG).t) = struct

  module Vec = Vector.Make (SpecG)

  type t = Vec.t
  type elt = SpecG.t

  let add = Vec.increment
  let union = fun a b -> Vec.union [a; b]

  let exists p v = List.exists p (Vec.elements v)

  let pp_t v = ""

  let empty = Vec.empty
  let mem e v = List.mem e @@ Vec.elements v

  let singleton e = Vec.of_word [e]

  let compare_elt : elt -> elt -> int = SpecG.compare_t
  let pp_elt      = fun _ -> ""

  let pp_elt_ws e = pp_elt e ^ " "
  
end

module BoolS : (Show with type t = bool) = struct
  type t = bool
  let pp = fun t -> if t then "true" else "false"
end

open Specs

(* module PIDsS (Vec : Vector_intf.VECTOR) : (Show with type t = Vec.t StrMap.t) = struct
  type t = Vec.t StrMap.t

  let pp = fun t -> ""
end *)

module AfterSetCheck    = Make (SpecC) (AfterSet)    (BoolS)
module VectorClockCheck (SpecC : Ideals_intf.SPEC) (SpecG : Cfg.Cfg_intf.SPEC with type t = SpecC.letter) =
  Make (SpecC) (VectorClock_Make (SpecG))