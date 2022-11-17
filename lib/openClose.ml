open Traversals
open AfterSetCheck
open Specs

module CSet = Set.Make(Char)

let word1 = ["o"; "x"; "c"; "o"; "x"; "c"]

let c1 : SpecC.conc_rel =
  fun c1 c2 -> 
    match c1, c2 with 
    | "x", _ | _, "x" -> false
    | _, _ -> true

type check = string -> acc_set -> bool -> bool

let locked_by o c (cr : SpecC.conc_rel) (l : string) (accs : acc_set) : bool -> bool = 
  if l = o then
    let after_o = LMap.find_opt o accs                                   in
    let cond    = 
      match after_o with 
      | None -> true
      | Some after_o -> AfterSet.exists (fun x -> not @@ cr c x) after_o in
      (&&) cond
  else Base.Fn.id

let be_added o c (cr : SpecC.conc_rel) (l : string) (accs : acc_set) : bool -> bool = 
  if l = c then
    let after_o = LMap.find_opt o accs                                   in
    let cond    = 
      match after_o with 
      | None -> true
      | Some after_o -> AfterSet.exists (fun x -> not @@ cr c x) after_o in
      (&&) cond
  else Base.Fn.id

let is_before o c (l : string) (accs : acc_set) : bool -> bool = 
  if l = c then
    let after_o = LMap.find_opt o accs                              in
    let after_o = Base.Option.value ~default:AfterSet.empty after_o in
    let cond    = AfterSet.mem o after_o                            in
      (&&) cond
  else Base.Fn.id

let rec all (fs : check list) =
  match fs with 
  | [f]     -> f
  | f :: fs -> fun l accs b -> f l accs b && all fs l accs b
  | []      -> failwith "!!!"


let open_close_check c : check = 
  all [
    (* locked_by 'o' 'c' c; *)
    (* locked_by 'c' 'o' c; *)
    be_added  "C" "O" c;
    be_added  "O" "C" c;
    (* is_before "o" "c" *)
  ]

(* let test1 =if true then Printf.printf "\n !!! \n";  traverse true word1 c1 open_close_check true *)
let check = fun w c -> traverse false w c (open_close_check c) true



(* let x = random_patterns_string ~rep:5 4 ~pattern:["o5"; "c"] *)
(* let c = random_rel_skeletom 10 *)
