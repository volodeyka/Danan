
module Make (SpecG : Cfg.Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) : 
  (Ideals_intf.IDEALS
    with module SpecC  := SpecC
    with module SpecG  := SpecG
    with module Vec    := Vector      .Make(SpecG)
    with module Gr     := Grammar_util.Make(SpecG)
    (* with module IntMap := Map         .Make(Int) *)
  )