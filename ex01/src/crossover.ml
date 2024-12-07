let length lst =
  let rec loop acc lst =
    match lst with [] -> acc | _ :: rest -> loop (acc + 1) rest
  in
  loop 0 lst

let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let filter p lst lst2 =
  let rec loop acc lst =
    match lst with
    | [] -> rev acc
    | first :: rest ->
        if p first lst2 then loop (first :: acc) rest else loop acc rest
  in
  loop [] lst

let contains n lst =
  let rec loop lst =
    match lst with
    | [] -> false
    | first :: rest -> if first = n then true else loop rest
  in
  loop lst

let crossover lst1 lst2 =
  match (lst1, lst2) with
  | [], _ -> []
  | _, [] -> []
  | _, _ ->
      if length lst1 > length lst2 then filter contains lst1 lst2
      else filter contains lst2 lst1

let () =
  let rec print_list_string l =
    match l with
    | head :: tail ->
        print_string head;
        print_list_string tail
    | _ -> print_char '\n'
  in
  print_list_string (crossover [ "a" ] [ "a" ]);
  print_list_string (crossover [ "a" ] [ "a"; "b" ]);
  print_list_string (crossover [ "a" ] [ "a"; "b"; "a" ]);
  print_list_string (crossover [ "a"; "b" ] [ "a" ]);
  print_list_string (crossover [ "a"; "b" ] [ "a"; "b" ]);
  print_list_string (crossover [ "a"; "b" ] [ "a"; "b"; "a" ]);
  print_list_string (crossover [ "a"; "b"; "a" ] [ "a" ]);
  print_list_string (crossover [ "a"; "b"; "a" ] [ "a"; "b" ]);
  print_list_string (crossover [ "a"; "b"; "a" ] [ "a"; "b"; "a" ]);
  print_list_string (crossover [ "a"; "b"; "a" ] []);
  print_list_string (crossover [] [ "a"; "b"; "a" ]);
  print_list_string (crossover [] []);
  print_list_string (crossover [ "a"; "b"; "a"; "c" ] [ "a"; "c" ])
