open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) = struct
  module Vec    = Vector      .Make(SpecG)
  module Cgf    = Cfg_impl    .Make(SpecG)
  module Gr     = Grammar_util.Make(SpecG)
  module Ideals = Ideals      .Make(SpecG)(SpecC)
  module ASet   = Set         .Make(struct type t = SpecG.t let compare = SpecG.compare_t end)
  module CSet   = Set         .Make(struct type t = ASet.t let compare = ASet.compare end)

  let rec insert (ws : 'a list)  (x : 'a) (i : int) : 'a list = 
    match ws with 
    | []           -> [x]
    | _ when i = 0 -> x :: ws
    | w :: ws      -> w :: insert ws x (i - 1)

  let permutations (w : Vec.word) : Vec.word list = 
    let rec premutations (ws : Vec.word) (l : int) : Vec.word list = 
      match ws with 
      | []     -> [[]]
      | w :: ws -> premutations ws (l - 1) |>
        Base.List.concat_map ~f:(fun u -> Base.List.init l ~f:(insert u w)) 
    in premutations w @@ List.length w |>
      Base.List.dedup_and_sort ~compare:compare

  (* module VMSet = CCMultiSet.Make (struct type t = Vec.t let compare = compare end) *)

  let equiv (c : SpecC.conc_rel) (w1 : Vec.word) (w2 : Vec.word) : bool = 
    List.equal (=)
      (List.sort compare (Ideals.of_word w1 c))
      (List.sort compare (Ideals.of_word w2 c))

  let equiv_aux (c : SpecC.conc_rel) (l : Ideals.t) (w2 : Vec.word) : bool = 
  List.equal Vec.equal
    l
    (List.sort Vec.compare (Ideals.of_word w2 c))

  let filter_equiv (c : SpecC.conc_rel) (w : Vec.word) = 
    let l = (List.sort Vec.compare (Ideals.of_word w c)) in
    Base.List.filter ~f:(equiv_aux c l)

  (* let all_equiv (c : SpecC.conc_rel) (w : Vec.word) : Vec.word list =  *)
    (* filter_equiv c w @@ permutations w *)

  let all_equiv (c : SpecC.conc_rel) (w : Vec.word) : Vec.word list = 
    let comp : Vec.word -> Vec.word -> int = (List.compare (SpecG.compare_t)) in
    let undup = Base.List.dedup_and_sort ~compare:comp  in
    let rec premutations (ws : Vec.word) (l : int) : Vec.word list = 
      match ws with 
      | []     -> [[]]
      | w :: ws ->  
        premutations ws (l - 1) |>
        undup                   |>
        filter_equiv c ws       |>
        Base.List.concat_map ~f:(fun u -> Base.List.init l ~f:(insert u w)) 
    in premutations w @@ List.length w |> undup |> filter_equiv c w

  let violate (c : SpecC.conc_rel) (w : Vec.word) (p : Vec.word -> bool) : bool = 
    all_equiv c w |> List.for_all p |> not

  let satisfy (c : SpecC.conc_rel) (w : Vec.word) (p : Vec.word -> bool) : bool = 
    all_equiv c w |> List.exists p

  let conc_rel_of (cs : CSet.t) : SpecC.conc_rel = 
    fun x y -> 
      SpecG.compare_t x y != 0 &&
      CSet.exists (fun s -> ASet.mem x s && ASet.mem y s) cs

  let cs_of (ls : SpecG.t list list) : CSet.t = 
    List.map ASet.of_list ls |> CSet.of_list

end

open Specs

module String_Naive_mltp = Make (SpecG) (SpecC)