let rec count l = match l with _ :: t -> 1 + count t | [] -> 0
let () = Printf.printf "%d\n" @@ count [ 4; 3; 1; 2; 6 ]
