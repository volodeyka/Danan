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
  
end

module type Show = sig
  type t
  val pp : t -> string
end

module Make (Spec : Ideals_intf.SPEC) (Acc : ACC with type elt = Spec.letter) (Show : Show) = struct

  module LMap   = Map.Make(struct type t = Spec.letter let compare = Acc.compare_elt end)

  type acc_set = Acc.t LMap.t
  type word    = Spec.letter list

  let update_dep (c : Spec.conc_rel) (base : Spec.letter) : Acc.t -> Acc.t = 
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

  let traverse 
  (verbose : bool)
  (w : word) 
  (c   : Spec.conc_rel)
  (upd : Spec.letter -> acc_set -> Show.t -> Show.t)
  (def : Show.t)
    : Show.t = 
    let rec trav (x : Show.t) (w : word) (accs : acc_set) = 
      match w with 
      | []     -> x
      | e :: w -> 
        (if verbose then Printf.printf "%s%!\n" @@ Show.pp x);
        (if verbose then Printf.printf "%s%!\n" @@ pp_acc_set accs);
        let x    = upd e accs x in
        let accs = 
          LMap.map (update_dep c e) accs |>
          LMap.add e (Acc.singleton e) 
        in trav x w accs
      in trav def w LMap.empty
end

module AfterSet : (ACC with type elt = string) = struct

  include Set.Make(String)
  let compare_elt : elt -> elt -> int = Base.String.compare
  let pp_elt      = Base.String.of_string

  let pp_elt_ws e = pp_elt e ^ " "
  let pp_t        = fun s -> Base.String.concat @@ List.map pp_elt_ws @@ elements s
  
end

module BoolS : (Show with type t = bool) = struct
  type t = bool
  let pp = fun t -> if t then "true" else "false"
end

module AfterSetCheck = Make (Specs.SpecC) (AfterSet) (BoolS)