let rec rev l = match l with h :: t -> rev t @ [ h ] | [] -> []

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

let () =
  List.iter
    (fun x ->
      let () = print_newline () in
      List.iter (fun y -> Printf.printf "%d\n" y) x)
    (pack [ 1; 2; 3; 3; 3; 3; 1; 1; 1; 2 ])
