open Vector_intf
open Cfg
module type SPEC = sig
  type letter

  type conc_rel = letter -> letter -> bool
  type alpha    = int

end

module type IDEALS = sig

  module SpecC   : SPEC
  module SpecG   : (Cfg_intf.SPEC with type t = SpecC.letter)

  module Vec     : (VECTOR with type letter := SpecC.letter)
  (* module IntMap  : (Map.S with type key = int) *)

  module Gr      : (Grammar_intf.GRAMMAR with module Spec := SpecG)

  type t   = Vec.t list
  type max_events = SpecG.t list
  type seq        = (Vec.t * max_events) list list

  val of_word  : Vec.word -> SpecC.conc_rel -> t

  (* val to_seq   : t -> SpecC.alpha -> seq *)
  val seq_to_q : seq -> Gr.t -> SpecG.nt list

  val to_string : t -> string
end