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
 
  
