open Vector_intf

module Make (Spec : Cfg.Cfg_intf.SPEC) : 
(VECTOR 
  with type letter   := Spec.t 
  and  type alphabet := (int, Spec.t) Hashtbl.t)