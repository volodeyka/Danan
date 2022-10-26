open Cfg.Cfg_intf

module type GRAMMAR = sig

  module Spec : SPEC    
  module Cfg  = Cfg.Cfg_impl.Make
  (* module Cfg  : (CFG with module Spec = Spec) *)

  type alphabet = (int, Spec.t) Hashtbl.t

  type word = Spec.t list

  type gr = {
    g        : Cfg(Spec).grammar;
    repr     : Cfg(Spec).ProdSet.t Cfg(Spec).NTMap.t;
    alphabet : alphabet
  }

  val from_grammar : Cfg(Spec).grammar -> gr
  val q : gr -> Spec.t list -> Spec.nt list
  val lambda : gr -> Spec.t -> Spec.nt list
  val sigma : gr -> Spec.t -> Spec.nt list -> Spec.nt list
end