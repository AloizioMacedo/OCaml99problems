let rec nth (l : 'a list) (n : int) : 'a option =
  match l with h :: t -> if n = 0 then Some h else nth t (n - 1) | [] -> None

let () = Printf.printf "%d\n" @@ Option.get @@ nth [ 4; 3; 1; 2; 6; 4 ] 2
