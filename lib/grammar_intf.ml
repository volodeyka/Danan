open Cfg.Cfg_intf

module type GRAMMAR = sig

  module Spec : SPEC    
  module Cfg  = Cfg.Cfg_impl.Make
  (* module Cfg  : (CFG with module Spec = Spec) *)

  open Cfg(Spec)

  type alphabet = (int, Spec.t) Hashtbl.t

  type word = Spec.t list

  type gr = {
    g        : grammar;
    repr     : ProdSet.t NTMap.t;
    alphabet : alphabet
  }

  val from_grammar : grammar -> gr
  val q : gr -> word -> Spec.nt list
  val lambda : gr -> Spec.t -> Spec.nt list
  val delta : gr -> Spec.t -> Spec.nt list -> Spec.nt list
end