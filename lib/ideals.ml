open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) = struct

  module Vec    = Vector      .Make(SpecG)
  module Cgf    = Cfg_impl    .Make(SpecG)
  module Gr     = Grammar_util.Make(SpecG)
  module MyGr   = Grammar_util.Make(SpecG)
  module IntMap = Map         .Make(Int)

  type t   = Vec.t list
  type seq = t IntMap.t

  let of_word  : Vec.word -> SpecC.conc_rel -> t = failwith "unimplemented"

  let to_seq   : t -> SpecC.alpha -> seq         = failwith "unimplemented"

  let seq_to_q : seq -> Gr.t -> SpecG.nt list    = failwith "unimpl"
  
end