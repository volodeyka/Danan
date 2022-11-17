open QCheck
open Utils

module QGen = struct
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

end

module RandGen = struct

  module Generator =
  Random_generator.Generator.Make(Random_generator.Prob_monad.Random)
open Generator

let random_string (len : int) = Gen.run Gen.(string (int 0 len Incl) (char 'a' 'f' Incl))
let random_string_wo_pattern (len : int) (pattern : string list) = 
  Base.List.fold pattern 
    ~f:(fun s p -> Base.String.substr_replace_all ~pattern:p ~with_:"" s)
    ~init:(random_string len)

let random_int1 () = Gen.(run @@ int 0 1 Incl)
let random_string1 i =
  if random_int1 () = 0 then
    Gen.run Gen.(string (return 1) (char 'a' 'f' Incl))
  else "o"
(* let random_string (len : int) = Gen.run Gen.(string (int 0 len Incl) (char 'a' 'z' Incl)) *)
let random_int (b : int) = Gen.(run @@ int 0 b Incl)

let  random_pattern_string (len : int) ~pattern = 
  let rec random_pattern_string ps = 
  match ps with 
  | []   -> random_string_wo_pattern len pattern
  | p :: ps -> 
    let ss = random_pattern_string ps in
    let s  = random_string_wo_pattern len pattern in 
      s ^ p ^ ss in random_pattern_string pattern


let random_patterns_string i ?rep (len : int) ~pattern = 
  let random_patterns_string = 
  match rep with 
  | None   -> random_pattern_string ~pattern:pattern len
  | Some n -> Base.List.init (random_int n) (random_pattern_string ~pattern:pattern) |> String.concat "" in 
  random_patterns_string |> Base.String.to_list |> List.map Base.Char.to_string

let rec random_take i (l : 'a list) : 'a list = 
  match (l : 'a list) with 
  | []   -> []
  | x :: l -> 
    if random_int1 () = 0 then 
      x :: random_take i l
    else random_take i l

open Utils
let random_rel_skeletom p w i (n : int) = 
  random_take i (all_sets p w 'a' 'f')

type tests = (string list list * string list list) list
  
let random_tests w ~test_number ~strings_number ~string_size ~rel_size ~rep ~pattern : tests = 
  test_number |> Base.List.init ~f:(
    fun i -> random_rel_skeletom pattern w i rel_size,
      strings_number |> Base.List.init ~f:(
        fun i -> random_patterns_string i ~rep string_size ~pattern
      )
  )
 

end



