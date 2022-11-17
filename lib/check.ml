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
    (* let rec pp_acc_set (s : (char * Acc.t) Seq.t) = 
      Seq. s in
    LMap.to_seq accs |> pp_acc_set *)

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
  (* let update (x, acc) event_i = 
    let check_event event_j event_j_vec new_vec = 
      if conc_rel event_i event_j then
        new_vec
      else
        Acc.merge new_vec event_j_vec
    in 
    let new_vec = 
      LMap.fold check_event clok_vec Vec.empty |>
      upd_clk_vec event_i in
    let clok_vec = 
      LMap.update event_i (fun _ -> Some new_vec) clok_vec in
    upd_struct event_i new_vec ideals, clok_vec
  in Base.List.fold run ~init:([], LMap.empty) ~f:update |> fst *)

end

(* module (Spec : Ideals_intf.SPEC) *)

module Spec : (Ideals_intf.SPEC with type letter = string) = struct

  type letter   = string

  type conc_rel = letter -> letter -> bool
  type alpha    = int

  let pp_letter = Base.String.of_string

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

module AfterSetCheck = Make (Spec) (AfterSet) (BoolS)

open AfterSetCheck

module CSet = Set.Make(Char)

let word1 = ["o"; "x"; "c"; "o"; "x"; "c"]

let c1 : Spec.conc_rel =
  fun c1 c2 -> 
    match c1, c2 with 
    | "x", _ | _, "x" -> false
    | _, _ -> true

type check = string -> acc_set -> bool -> bool

let locked_by o c (cr : Spec.conc_rel) (l : string) (accs : acc_set) : bool -> bool = 
  if l = o then
    let after_o = LMap.find_opt o accs                                   in
    let cond    = 
      match after_o with 
      | None -> true
      | Some after_o -> AfterSet.exists (fun x -> not @@ cr c x) after_o in
      (&&) cond
  else Base.Fn.id

let be_added o c (cr : Spec.conc_rel) (l : string) (accs : acc_set) : bool -> bool = 
  if l = c then
    let after_o = LMap.find_opt o accs                                   in
    let cond    = 
      match after_o with 
      | None -> true
      | Some after_o -> AfterSet.exists (fun x -> not @@ cr c x) after_o in
      (&&) cond
  else Base.Fn.id

let is_before o c (l : string) (accs : acc_set) : bool -> bool = 
  if l = c then
    let after_o = LMap.find_opt o accs                              in
    let after_o = Base.Option.value ~default:AfterSet.empty after_o in
    let cond    = AfterSet.mem o after_o                            in
      (&&) cond
  else Base.Fn.id

let rec all (fs : check list) =
  match fs with 
  | [f]     -> f
  | f :: fs -> fun l accs b -> f l accs b && all fs l accs b
  | []      -> failwith "!!!"


let open_close_check c : check = 
  all [
    (* locked_by 'o' 'c' c; *)
    (* locked_by 'c' 'o' c; *)
    be_added  "C" "O" c;
    be_added  "O" "C" c;
    (* is_before "o" "c" *)
  ]

(* let test1 =if true then Printf.printf "\n !!! \n";  traverse true word1 c1 open_close_check true *)
let check_oc = fun w c -> traverse false w c (open_close_check c) true
open Gen



(* let x = random_patterns_string ~rep:5 4 ~pattern:["o5"; "c"] *)
(* let c = random_rel_skeletom 10 *)
