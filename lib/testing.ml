open Cfg
open Naive_mltp
open Generator
open Specs
open RandGen
open String_Naive_mltp

let word1 = ["o"; "x"; "c"; "o"; "x"; "c"]
let word2 = ["o"; "x"; "o"; "c"; "x"; "c"]

let c1 : SpecC.conc_rel =
  fun c1 c2 -> 
    match c1, c2 with 
    | "x", _ | _, "x" -> false
    | _, _ -> true



let random_test1
    (f : BnfVector.word -> SpecC.conc_rel -> bool) 
    (p : BnfVector.word -> bool) 
    ((c, ws) : string list list * string list list) = 
  let c = c |> cs_of |> conc_rel_of in
  let rec random_tests1 (ws : string list list) =
    match ws with 
    | []   -> true 
    |  w :: ws -> 
      Printf.printf "one more test: %s\n%!" (String.concat "" w);
      if not (violate c w p) = f w c then 
        random_tests1 ws
      else (Printf.printf "Test failed%!"; false)
  in random_tests1 ws

  (* Base.List.for_all ws ~f:(
    fun w -> 
  ) *)

let print_rel (ss : string list list) =
  Base.List.iter ss ~f:(fun ss -> Printf.printf "[%!"; Base.List.iter ~f:(Printf.printf "%s %!; ") ss; Printf.printf "]%!");
  Printf.printf "\n%!"

let random_test 
  (f : BnfVector.word -> SpecC.conc_rel -> bool) 
  (p : BnfVector.word -> bool) 
  (ts : RandGen.tests) = 
  let num = List.length ts in
  let rec random_test (i : int) ts = 
    match ts with 
    | []   -> true
    | t :: ts ->
      print_rel (fst t);
      if random_test1 f p t then
        (Printf.printf "  passed %d/%d\n%!" i num;
        random_test (i + 1) ts)
      else false
  in random_test 1 ts

let check_oc_p (ws : BnfVector.word) = 
  let rec check_oc_p f (ws : BnfVector.word) = 
    match ws with 
    | []      -> f 
    | w :: ws -> 
      match w, f with 
      | "C", true  -> false
      | "O", false -> false
      | "C", false -> check_oc_p true  ws
      | "O", true  -> check_oc_p false ws
      | _ ,  _     -> check_oc_p f     ws
  in check_oc_p true ws

let tests = random_tests 
  ~test_number:    100
  ~strings_number: 50
  ~string_size:    4  
  ~rel_size:       30
  ~rep:            3
  ~pattern:        ["o"; "c"];;

let c = 
  [["O"; "C"]; ["b"; "O"]; ["c"; "O"]; ["e"; "O"]; ["f"; "C"]; ["f"; "d"]; ] |> cs_of |> conc_rel_of

(* let w = ["o"; "x"; "c"; "o"; "x"; "c"] *)


let w =  List.map (Base.Char.to_string) @@ Base.String.to_list "ObfbC"

let b = violate c w check_oc_p
let b' = OpenClose.check w c

(* let eq = *)


open QGen

let test_eqf a z
  ~f1
  ~f2
  ~test_number
  ~string_size
  ~strings_number
  ~rel_size
  ~rep = 
  QCheck.Test.make ~count:test_number (arbitrary_test a z ~string_size ~strings_number ~rel_size ~rep ~pattern:["C"; "O"])
  (fun (c, w) -> 
        let w = List.map (Base.Char.to_string) @@ Base.String.to_list w in 
        let c = c |> cs_of |> conc_rel_of in
        f1 c w = f2 c w
    )








