
module Make (SpecG : Cfg.Cfg_intf.SPEC) (SpecC : Pid_intf.SPEC with type letter = SpecG.t) : 
  (Pid_intf.PID 
    with module Spec := SpecC
    and  module Vec  := Vector.Make(SpecG))