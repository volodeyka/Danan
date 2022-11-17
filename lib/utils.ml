let all_between p w (c1 : char) (c2 : char) = 
  let rec rel_arr x = 
    if Base.Char.between ~low:c1 ~high:c2 x then
      Base.String.of_char x :: 
      (Base.Char.to_int x + 1 |> Base.Char.of_int |> Base.Option.value ~default:c1 |> rel_arr)
    else [] in List.filter (fun l -> List.mem l w) @@ p @ rel_arr c1

let rec undup ~equiv (ls : 'a list) =
  match ls with 
  | []   -> []
  | l :: ls -> if Base.List.mem ls l equiv then undup ~equiv ls else l :: undup ~equiv ls

let equiv (ls1 : string list) (ls2 : string list) = 
  match ls1, ls2 with
  | [x1; x2], [y1; y2] -> x1 = y1 && x2 = y2 || x1 = y2 && x2 = y1
  | _       , _        -> false

let diag (ls : string list) = 
  match ls with
  | [x1; x2] -> x1 = x2 |> not
  | _        -> true

let all_sets p w (c1 : char) (c2 : char) = 
  List.filter diag @@
  undup ~equiv:equiv @@ List.flatten @@
  let open Base.List.Let_syntax in 
  all_between p w c1 c2 >>| fun x ->
  all_between p w c1 c2 >>| fun y ->
    [x ; y]