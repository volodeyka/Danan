open Cfg
open Bnf_spec
open Grammar_intf

module BnfVector = (Vector.Make (Bnf.Spec))
module Bnf = Cfg.Cfg_impl.Make (Bnf.Spec)

let rec rule_of (s : string) : Spec.symbol list = 
  begin match s with 
  | "" -> []
  | _  -> 
    let h = String.sub s 0 1                     in
    let t = String.sub s 1 (String.length s - 1) in
    (if String.uppercase_ascii h = h then
      Spec.NT h 
    else Spec.T h) :: rule_of t
  end

let (-->) : Spec.nt -> string -> Bnf.grammar -> Bnf.grammar =
  fun nt s g -> Bnf.add_prod g nt () (rule_of s)

let gr_ab : Bnf.grammar = 
  "S" --> "bA" @@
  "B" --> "bA" @@
  "A" --> "aB" @@
  "B" --> "b"  @@
  "A" --> "a"  @@
  Bnf.empty

let w = Bnf.add_prod gr_ab

module Gr = BnfVector.Gr

let x : BnfVector.t = 
  let open BnfVector in
  AMap.add "a" 2 AMap.empty

let y = BnfVector.to_letter (Gr.from_grammar gr_ab) x
