type 'a node = One of 'a | Many of 'a node list

let flatten (node : 'a node) : 'a list =
  let rec flatten' (node' : 'a node) (acc : 'a list) : 'a list =
    match node' with
    | One x -> acc @ [ x ]
    | Many l -> (
        match l with
        | h :: t ->
            flatten' h acc
            @ List.fold_left (fun acc x -> acc @ flatten' x []) [] t
        | _ -> [])
  in

  flatten' node []

let my_node = Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ]
let () = List.iter (fun x -> Printf.printf "%s-" x) @@ flatten my_node
let () = print_newline ()
