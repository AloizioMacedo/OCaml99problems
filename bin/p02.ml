let rec last_but_one l =
  match l with [ x; y ] -> Some (x, y) | _ :: t -> last_but_one t | _ -> None

let x, y = Option.get @@ last_but_one [ 1; 3; 5; 8; 2 ]
let () = Printf.printf "Second to last: %d, Last: %d\n" x y
