let rec rev l = match l with h :: t -> rev t @ [ h ] | [] -> []
let rec count l = match l with _ :: t -> 1 + count t | [] -> 0

let pack (l : 'a list) : 'a list list =
  let rec pack' (l' : 'a list) (acc : 'a list list) : 'a list list =
    match l' with
    | h :: t -> (
        match acc with
        | (h_in :: t_in) :: t_acc ->
            if h = h_in then pack' t ((h :: h_in :: t_in) :: t_acc)
            else pack' t ([ h ] :: (h_in :: t_in) :: t_acc)
        | _ -> pack' t ([ [ h ] ] @ acc))
    | [] -> acc
  in

  rev @@ pack' l []

let encode (l : 'a list) : (int * 'a) list =
  let packed = pack l in

  let rec encode' (l : 'a list list) : (int * 'a) list =
    match l with
    | (h :: _ as inner) :: t -> (count inner, h) :: encode' t
    | [] -> []
    | _ -> failwith "encoding failed"
  in

  encode' packed

let () =
  List.iter (fun (c, d) -> Printf.printf "Amount: %d, Value: %d\n" c d)
  @@ encode [ 1; 1; 1; 3; 2; 2; 2; 4; 4; 2; 2 ]
