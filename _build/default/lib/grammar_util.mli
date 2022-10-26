open Grammar_intf
open Cfg.Cfg_intf

module Make (Spec : SPEC) : 
(GRAMMAR with module Spec := Spec)
