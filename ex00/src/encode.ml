let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let encode lst =
  let rec loop acc i c lst =
    match lst with
    | [] -> rev ((i, c) :: acc)
    | first :: rest ->
        if first = c then loop acc (i + 1) first rest
        else loop ((i, c) :: acc) 1 first rest
  in

  match lst with [] -> [] | first :: rest -> loop [] 1 first rest

let print_lst lst =
  List.iter (fun (count, str) -> Printf.printf "(%d, %s) " count str) lst;
  print_newline ()

let () =
  print_lst (encode [ "a"; "a"; "a"; "b"; "b"; "b" ]);
  print_lst (encode [ "a"; "a"; "a"; "b"; "b"; "c" ]);
  print_lst (encode [])
