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

module SpecG     = Bnf_spec.Bnf.Spec

module BnfVector = Vector      .Make (SpecG)
module BnfGr     = Grammar_util.Make (SpecG)
module Bnf       = Cfg.Cfg_impl.Make (SpecG)
module IntMap    = Map         .Make (Int)
  
module SpecC : (Ideals_intf.SPEC with type letter = SpecG.t) = struct
  type letter   = SpecG.t 
  type conc_rel = letter -> letter -> bool
  type alpha    = int
  
  let pp_letter = fun l -> l
end



module Testing = Make (SpecG) (SpecC)

open Testing
open Gen

let word1 = ["o"; "x"; "c"; "o"; "x"; "c"]
let word2 = ["o"; "x"; "o"; "c"; "x"; "c"]

let c1 : SpecC.conc_rel =
  fun c1 c2 -> 
    match c1, c2 with 
    | "x", _ | _, "x" -> false
    | _, _ -> true



let random_test1
    (f : BnfVector.word -> SpecC.conc_rel -> bool) 
    (p : BnfVector.word -> bool) 
    ((c, ws) : string list list * string list list) = 
  let c = c |> cs_of |> conc_rel_of in
  let rec random_tests1 (ws : string list list) =
    match ws with 
    | []   -> true 
    |  w :: ws -> 
      Printf.printf "one more test: %s\n%!" (String.concat "" w);
      if not (violate c w p) = f w c then 
        random_tests1 ws
      else (Printf.printf "Test failed%!"; false)
  in random_tests1 ws

  (* Base.List.for_all ws ~f:(
    fun w -> 
  ) *)

let print_rel (ss : string list list) =
  Base.List.iter ss ~f:(fun ss -> Printf.printf "[%!"; Base.List.iter ~f:(Printf.printf "%s %!; ") ss; Printf.printf "]%!");
  Printf.printf "\n%!"

let random_test 
  (f : BnfVector.word -> SpecC.conc_rel -> bool) 
  (p : BnfVector.word -> bool) 
  (ts : Gen.tests) = 
  let num = List.length ts in
  let rec random_test (i : int) ts = 
    match ts with 
    | []   -> true
    | t :: ts ->
      print_rel (fst t);
      if random_test1 f p t then
        (Printf.printf "  passed %d/%d\n%!" i num;
        random_test (i + 1) ts)
      else false
  in random_test 1 ts

let check_oc_p (ws : BnfVector.word) = 
  let rec check_oc_p f (ws : BnfVector.word) = 
    match ws with 
    | []      -> f 
    | w :: ws -> 
      match w, f with 
      | "C", true  -> false
      | "O", false -> false
      | "C", false -> check_oc_p true  ws
      | "O", true  -> check_oc_p false ws
      | _ ,  _     -> check_oc_p f     ws
  in check_oc_p true ws

let tests = random_tests 
  ~test_number:    100
  ~strings_number: 50
  ~string_size:    4  
  ~rel_size:       30
  ~rep:            3
  ~pattern:        ["o"; "c"];;

let c = 
  [["O"; "C"]; ["b"; "O"]; ["c"; "O"]; ["e"; "O"]; ["f"; "C"]; ["f"; "d"]; ] |> cs_of |> conc_rel_of

(* let w = ["o"; "x"; "c"; "o"; "x"; "c"] *)


let w =  List.map (Base.Char.to_string) @@ Base.String.to_list "ObfbC"

let b = violate c w check_oc_p
let b' = Check.check_oc w c

(* let eq = *)


open Qgen

(* QCheck.Gen. *)

let test_oc a z
  ~test_number
  ~string_size
  ~strings_number
  ~rel_size
  ~rep = 
  QCheck.Test.make ~count:test_number (arbitrary_test a z ~string_size ~strings_number ~rel_size ~rep ~pattern:["C"; "O"])
  (fun (c, w) -> 
        let w = List.map (Base.Char.to_string) @@ Base.String.to_list w in 
        let c = c |> cs_of |> conc_rel_of in
        not (violate c w check_oc_p) = Check.check_oc w c
    )








