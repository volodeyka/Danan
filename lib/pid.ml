open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Pid_intf.SPEC with type letter = SpecG.t) = struct

  module Vec = Vector.Make(SpecG)
  module Cgf = Cfg_impl.Make(SpecG)
  module Gr  = Grammar_util.Make(SpecG)

  type pids  = Vec.t list

  let of_word : Vec.word -> pids = failwith "unimplemented"
end