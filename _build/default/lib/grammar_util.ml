
open Cfg.Cfg_intf
open Grammar_intf

module Make (Spec : SPEC) : (GRAMMAR with module Spec := Spec) = struct
  (* module Spec  = Spec_ *)
  module Cfg   = Cfg.Cfg_impl.Make
  module MyCfg = Cfg(Spec)

open MyCfg

type alphabet = (int, Spec.t) Hashtbl.t  

type gr = {
  g        : grammar;
  repr     : ProdSet.t NTMap.t;
  alphabet : alphabet
}

type word = Spec.t list



let add_aphabet (a : alphabet) (g : grammar) = 
  let _ = List.mapi 
    (fun i x -> Hashtbl.add a i x)
    (g |> ts_in_grammar |> TSet.elements) in ()

let from_grammar (gr : grammar) : gr =
  {
    g        = gr;
    repr     = grammar_contents gr;
    alphabet = 
      let h = Hashtbl.create 2 in
      add_aphabet h gr;
      h
  }

let rec q (g : gr) (xs : Spec.t list) : Spec.nt list = 
  match xs with  
  | [x]  -> 
    NTMap.fold 
      (fun nt ps l -> 
        if (ProdSet.exists (fun (_, s) -> s = [Spec.T x]) ps) then 
          nt :: l
        else l
      ) g.repr [] 
  | a :: x' -> 
    let qx' = q g x' in 
    NTMap.fold 
      (fun nt ps l -> 
        if (ProdSet.exists (fun (_, s) -> 
          match s with 
          | [Spec.T a'; Spec.NT b] when a' = a -> List.mem b qx'
          | _    -> false 
          ) ps) then 
          nt :: l
        else l
      ) g.repr []
  | _    -> failwith "???"

let lambda (g : gr) (x : Spec.t) : Spec.nt list = 
  q g [x]

let sigma (g : gr) (x : Spec.t) (bs : Spec.nt list) : Spec.nt list = 
  NTMap.fold (
    fun nt ps l ->
      if (ProdSet.exists (fun (_, s) -> 
        match s with 
        | [Spec.T a; Spec.NT b] when x = a -> List.mem b bs
        | _    -> false 
        ) ps) then 
        nt :: l
      else l
  ) g.repr []


end