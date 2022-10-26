open Cfg.Cfg_intf

module type GRAMMAR = sig

  module Spec : SPEC    
  module Cfg  = Cfg.Cfg_impl.Make
  (* module Cfg  : (CFG with module Spec = Spec) *)

  open Cfg(Spec)

  type alphabet = (int, Spec.t) Hashtbl.t

  type word = Spec.t list

  type t = {
    g        : grammar;
    repr     : ProdSet.t NTMap.t;
    alphabet : alphabet
  }

  val from_grammar : grammar -> t
  val q : t -> word -> Spec.nt list
  val lambda : t -> Spec.t -> Spec.nt list
  val delta : t -> Spec.t -> Spec.nt list -> Spec.nt list
end