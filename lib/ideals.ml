open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) = struct

  module Vec    = Vector      .Make(SpecG)
  module Cgf    = Cfg_impl    .Make(SpecG)
  module Gr     = Grammar_util.Make(SpecG)
  module VecMap = Map         .Make(struct type t = Vec.t let compare = compare end)

  type t          = Vec.t   list
  type max_events = SpecG.t list
  type seq        = (Vec.t * max_events) list list

  let of_word  : Vec.word -> SpecC.conc_rel -> t = failwith "unimplemented"

  let to_seq   : t -> SpecC.alpha -> seq         = failwith "unimplemented"

  type x = SpecG.nt list VecMap.t
  
  let rec ids1_to_q (gr : Gr.t) (ids : t) (x : x) : x = 
    match ids with 
    | []   -> VecMap.empty 
    | v :: ids -> 
      let word = Vec.to_letter gr.alphabet v in 
      let qv   = Gr.lambda gr word           in
      VecMap.add v qv x

  (* let rec ids_to_q  *)

  let seq_to_q (s : seq) (gr : Gr.t) (ns : SpecG.nt list) : SpecG.nt list = 
    let word_repr =
      match Base.List.last s with
      | Some [(v, _)] -> v 
      | _        -> failwith "incorrect seq"
    in
    let s, x_init = 
      begin match s with 
      | []        -> s, VecMap.empty (* !!! *)
      | ids1 :: s -> s, ids1_to_q gr (List.map fst ids1) VecMap.empty
      end
    in
    let rec seq_to_q (s : seq) (x : x) : x =
      match s with 
      | []      -> x
      | vs :: s -> failwith "unimpl" 
    in VecMap.find word_repr (seq_to_q s x_init)


  
end