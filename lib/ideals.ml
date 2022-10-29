open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) = struct

  module Vec    = Vector      .Make(SpecG)
  module Cgf    = Cfg_impl    .Make(SpecG)
  module Gr     = Grammar_util.Make(SpecG)
  module VecMap = Map         .Make(struct type t = Vec.t let compare = compare end)

  type t          = Vec.t   list
  type max_events = SpecG.t list
  type seq        = (Vec.t * max_events) list list
  type p          = (Vec.t * max_events) list

  let of_word  : Vec.word -> SpecC.conc_rel -> t = failwith "unimplemented"

  let to_seq   : t -> SpecC.alpha -> seq         = failwith "unimplemented"

  type x = SpecG.nt list VecMap.t

  (******* Final Algorithm Implementation *******)

  (* construct X function for one-element prefix representations (P(1)) *)
  let rec p1_to_q (gr : Gr.t) (p1 : p) (x : x) : x = 
    match p1 with 
    | []           -> VecMap.empty 
    | (v, _) :: p1 -> 
      let sigma = Vec.to_letter gr.alphabet v in 
      let qv    = Gr.lambda gr sigma          in
      p1_to_q gr p1 x |> VecMap.add v qv

  (* construct X function for other prefixes representation (P(k), where 2 <= k <= |x|) *)
  let rec ps_to_q (gr : Gr.t) (ps : p) (x : x) : x = 
    match ps with 
    | []   -> x
    | (v, max) :: ps -> 
      let x = ps_to_q gr ps x in
      List.fold_left 
        (fun x e -> 
          let v'  = Vec.delete e v    in
          let xv' = VecMap.find v' x  in
          let qv  = Gr.delta gr e xv' in
          VecMap.add v qv x) 
      x max

  (* building all together *)
  let seq_to_q (s : seq) (gr : Gr.t) : SpecG.nt list = 
    let word_repr =
      match Base.List.last s with
      | Some [(v, _)] -> v 
      | _             -> failwith "incorrect seq"
    in
    let ps, x_init = 
      begin match s with 
      | []        -> s , VecMap.empty (* !!! *)
      | p1 :: ps  -> ps, p1_to_q gr p1 VecMap.empty
      end
    in
    let rec seq_to_q (s : seq) (x : x) : x =
      match s with 
      | []      -> x
      | vs :: s -> ps_to_q gr vs x |> seq_to_q s
    in VecMap.find word_repr (seq_to_q s x_init)


  
end