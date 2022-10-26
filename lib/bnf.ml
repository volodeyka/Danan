open Cfg
(* open Bnf_spec *)
(* open Grammar_intf *)


module SpecG     = Bnf_spec.Bnf.Spec

module BnfVector = Vector      .Make (SpecG)
module BnfGr     = Grammar_util.Make (SpecG)
module Bnf       = Cfg.Cfg_impl.Make (SpecG)
module IntMap    = Map         .Make(Int)

module SpecC : (Ideals_intf.SPEC with type letter = SpecG.t) = struct
  type letter   = SpecG.t 
  type conc_rel = letter -> letter -> bool
  type alpha    = int
end

module BnfIdeals = Ideals.Make(SpecG)(SpecC)

let rec rule_of (s : string) : SpecG.symbol list = 
  begin match s with 
  | "" -> []
  | _  -> 
    let h = String.sub s 0 1                     in
    let t = String.sub s 1 (String.length s - 1) in
    (if String.uppercase_ascii h = h then
      SpecG.NT h 
    else SpecG.T h) :: rule_of t
  end

let (-->) : SpecG.nt -> string -> Bnf.grammar -> Bnf.grammar =
  fun nt s g -> Bnf.add_prod g nt () (rule_of s)

let gr_ab : Bnf.grammar = 
  "S" --> "bA" @@
  "B" --> "bA" @@
  "A" --> "aB" @@
  "B" --> "b"  @@
  "A" --> "a"  @@
  Bnf.empty

let cr : SpecC.conc_rel = fun _ _ -> false
let a  : SpecC.alpha    = 0

let gr = BnfGr.from_grammar gr_ab

let y = BnfVector.to_letter gr.alphabet

let w : BnfVector.word = ["x"; "y"]

let s : BnfIdeals.seq = IntMap.empty

let _ = BnfIdeals.of_word w cr
let _ = BnfIdeals.seq_to_q s gr




