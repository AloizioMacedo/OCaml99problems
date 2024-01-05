let rec rev l = match l with h :: t -> rev t @ [ h ] | [] -> []
let () = List.iter (fun x -> Printf.printf "%d\n" x) @@ rev [ 5; 3; 2; 8 ]
