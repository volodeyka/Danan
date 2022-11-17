open QCheck
open Utils

let gen_small_str a z ~strings = Gen.string_size ~gen:(Gen.char_range a z) Gen.(0 -- strings)

let gen_oc_string a z ~strings ~rep = Gen.(sized_size (int_bound rep) @@ fix (
  fun sefl n -> 
    match n with
    | 0 -> (fun s -> "O" ^ s ^ "C") <$> gen_small_str a z ~strings
    | _ -> map2 (^) (sefl 0) (sefl (n - 1))
))

let arbitrary_oc_string a z ~strings ~rep = 
  make ~print:Base.Fn.id @@ gen_oc_string a z ~strings ~rep

(* let a = Gen.generate ~n:20 gen_oc_string *)

let gen_rel w a z ~rel_size ~pattern = 
  let open Gen in 
  let full_conc = all_sets pattern w a z in 
  let len = List.length full_conc        in
  let  n = Base.Int.min rel_size len     in
  let* n = Gen.int_bound n               in
    full_conc
    |> Array.of_list
    |> Gen.array_subset n
    |> Gen.map (fun x -> ["O"; "C"] :: Array.to_list x)

let rec print_rel (ss : string list list) =
  match ss with 
  | [] -> "]"
  | [a; b] :: r -> "[" ^ a ^ "; " ^ b ^ "]; " ^ print_rel r
  | _ -> "??"

let rec shrink_delete (ss : 'a list) = 
  let open Iter in
  match ss with 
  | x :: l -> (let+ sl = shrink_delete l in x :: sl) <+> return l
  | [] -> return []


let arbitrary_rel w a z ~rel_size ~pattern = make ~shrink:shrink_delete ~print:print_rel @@ gen_rel w a z ~rel_size ~pattern

let shrink_test = 
  let l w = List.map (Base.Char.to_string) @@ Base.String.to_list w in 
  Shrink.pair (shrink_delete) (fun w -> Iter.map (String.concat "") @@ shrink_delete (l w))

let gen a z ~strings ~rep ~rel_size ~strings_number ~pattern = 
  let open Gen in
  let* l = gen_oc_string a z ~strings ~rep in
  let l' = List.map (Base.Char.to_string) @@ Base.String.to_list l in
  Gen.pair (gen_rel l' a z ~pattern ~rel_size) (return l)


let print_test (r, w) = "[" ^ print_rel r ^ "-->" ^ w

let arbitrary_test a z ~string_size ~rep ~strings_number ~rel_size ~pattern = make ~print:print_test ~shrink:shrink_test @@ gen a z ~strings:string_size ~rep ~strings_number ~rel_size ~pattern


