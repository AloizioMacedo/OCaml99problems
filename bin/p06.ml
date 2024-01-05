let rec rev l = match l with h :: t -> rev t @ [ h ] | [] -> []
let is_palindrome l = l = rev l

let () =
  Printf.printf "Is palindrome? %b\n" @@ is_palindrome [ 1; 4; 2; 10; 2; 4; 1 ]

let () =
  Printf.printf "Is palindrome? %b\n" @@ is_palindrome [ 1; 4; 2; 10; 2; 4 ]
