open Cfg

module Make (SpecG : Cfg_intf.SPEC) (SpecC : Ideals_intf.SPEC with type letter = SpecG.t) = struct

  module Vec    = Vector      .Make(SpecG)
  module Cgf    = Cfg_impl    .Make(SpecG)
  module Gr     = Grammar_util.Make(SpecG)
  module VecMap = Map         .Make(struct type t = Vec.t let compare = compare end)
  module LMap   = Map         .Make(struct type t = SpecG.t let compare = SpecG.compare_t end)

  type t          = Vec.t   list
  type max_events = SpecG.t list
  type seq        = (Vec.t * max_events) list list
  type p          = (Vec.t * max_events) list
  type clk_vec    = Vec.t LMap.t

  let to_string (ideals : t) : string = 
    let ideal_to_string str ideal = 
      Vec.to_string ideal ^ "x\n" ^ str
    in
    (Base.List.length ideals) |> print_int;
    Base.List.fold ideals ~init:"" ~f:ideal_to_string

    
  let of_word (run : Vec.word) (conc_rel : SpecC.conc_rel) : t = 
    let update ((ideals : t), (clok_vec : clk_vec)) event_i = 
      let check_event event_j event_j_vec new_vec = 
        if conc_rel event_i event_j then
          new_vec
        else
          Vec.union [new_vec; event_j_vec]
      in 
      let new_vec = (LMap.fold check_event clok_vec Vec.empty) |>
                    (Vec.increment event_i) in
      let clok_vec = LMap.update event_i (fun x -> Some(new_vec)) clok_vec in
      (new_vec :: ideals, clok_vec)
    in Base.List.fold run ~init:([], LMap.empty) ~f:update |> fst


  let traverse 
    (run         : Vec.word) 
    (conc_rel    : SpecC.conc_rel)
    (upd_clk_vec : SpecG.t -> Vec.t -> Vec.t)
    (upd_struct  : SpecG.t -> Vec.t -> 'a -> 'a)
   : 'a = 
    let update ((ideals : t), (clok_vec : clk_vec)) event_i = 
      let check_event event_j event_j_vec new_vec = 
        if conc_rel event_i event_j then
          new_vec
        else
          Vec.union [new_vec; event_j_vec]
      in 
      let new_vec = 
        LMap.fold check_event clok_vec Vec.empty |>
        upd_clk_vec event_i in
      let clok_vec = 
        LMap.update event_i (fun _ -> Some new_vec) clok_vec in
      upd_struct event_i new_vec ideals, clok_vec
    in Base.List.fold run ~init:([], LMap.empty) ~f:update |> fst

  (* let to_seq   : t -> SpecC.alpha -> seq         = failwith "unimplemented" *)

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