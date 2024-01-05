let rec last l = match l with [ x ] -> Some x | _ :: t -> last t | [] -> None
let () = Printf.printf "%d\n" @@ Option.get @@ last [ 1; 5; 3; 2 ]
