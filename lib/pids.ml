open Core
open Stdio

module SS = Set.Make(Int)
include SS

let stringset_of_list l =
  List.fold_right l ~f:(fun elem set -> SS.add set elem) ~init:SS.empty 

(* Sigma and Concurrent Relation *)
let sigma = ["a"; "b"; "c"; "d"; "e"]
let sigma_id = 
  let id = Hashtbl.create (module String) ~size:(List.length sigma) in
  let _ = List.iteri sigma ~f:(fun cnt letter -> Hashtbl.set id ~key:letter ~data:cnt) in
  id
let sigma_length = List.length sigma
let concRel letter = 
  match letter with
  | 0 -> stringset_of_list []
  | 1 -> stringset_of_list [2; 4]
  | 2 -> stringset_of_list [1]
  | 3 -> stringset_of_list [4]
  | 4 -> stringset_of_list [1; 3]
  | _   -> SS.empty

let extract_event_id event = match Hashtbl.find sigma_id event with
  | Some(id) -> id
  | None -> failwith "not such event"

(* Clock Vector Algorithm *)
let update (ideals, vec) event = 
  let event_id = extract_event_id event in
  let check_event (new_vec, event_j_id) event_j_vec = 
    if SS.mem (concRel event_id) event_j_id then
      (new_vec, event_j_id + 1)
    else 
      (Array.map2_exn event_j_vec new_vec ~f:max, event_j_id + 1)
  in
  let (new_vec, _) = Array.fold vec ~init:(vec.(event_id), 0) ~f:check_event in
  let _ = new_vec.(event_id) <- new_vec.(event_id) + 1 in
  let _ = vec.(event_id) <- new_vec in
  (ideals@[new_vec], vec)


let principal_ideal trace = 
  let clkVec = Array.init sigma_length ~f:(fun _ -> Array.create ~len:sigma_length 0) in
  let (ideals, _) = List.fold trace ~init:([], clkVec) ~f:update in 
  ideals

let print_ideals ideals = 
  let print_ideal vec = 
    let _ = Array.iter vec ~f:(fun i -> printf "%d " i) in
    printf "\n"
  in
  List.iter ideals ~f:print_ideal

let () = print_ideals (principal_ideal ["a"; "b"; "c"; "d"; "e"; "a"; "c"])
