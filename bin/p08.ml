let compress (l : 'a list) : 'a list =
  let rec compress' (l' : 'a list) (last : 'a option) : 'a list =
    match last with
    | Some last -> (
        match l' with
        | h :: t ->
            if h = last then compress' t (Some h)
            else [ h ] @ compress' t (Some h)
        | _ -> [])
    | None -> (
        match l' with h :: t -> [ h ] @ compress' t (Some h) | _ -> [])
  in

  compress' l None

let () =
  List.iter (fun x -> Printf.printf "%d-" x)
  @@ compress [ 1; 1; 1; 1; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 5; 5; 5; 6 ]

let () = print_newline ()
