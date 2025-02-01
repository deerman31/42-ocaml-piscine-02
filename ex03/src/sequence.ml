let contains n lst =
  let rec loop lst =
    match lst with
    | [] -> false
    | first :: rest -> if first = n then true else loop rest
  in
  loop lst

let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let length lst =
  let rec loop acc lst =
    match lst with [] -> acc | _ :: rest -> loop (acc + 1) rest
  in
  loop 0 lst

let length_lst_add lst =
  let hd lst = match lst with [] -> 0 | first :: _ -> first in
  length lst :: [ hd lst ]

let lsts_map f lsts =
  let rec loop acc lsts =
    match lsts with
    | [] -> rev acc
    | first :: rest -> loop (f first :: acc) rest
  in
  loop [] lsts

let rec append lst1 lst2 =
  match lst1 with [] -> lst2 | first :: rest -> first :: append rest lst2

let concat lsts =
  let rec loop acc lsts =
    match lsts with [] -> acc | first :: rest -> loop (append first acc) rest
  in
  loop [] (rev lsts)

let same_split_lst lst =
  let rec loop acc1 acc2 lst =
    match lst with
    | [] -> rev (acc2 :: acc1)
    | first :: rest ->
        if acc1 = [] && acc2 = [] then loop acc1 [ first ] rest
        else if contains first acc2 then loop acc1 (first :: acc2) rest
        else loop (acc2 :: acc1) [ first ] rest
  in
  loop [] [] lst

let run_len_encode lst =
  (*同じ数字ごとにlstにまとめ、int listのlistを作成する*)
  let lsts = same_split_lst lst in

  (* listの長さを調べ、その長さとlstの要素一個のlistを作成 [[1 ; 1 ; 1 ; 1] ; [3 ; 3]] -> [[4 ; 1] ; [2 ; 3]]*)
  let new_lsts = lsts_map length_lst_add lsts in

  (* listのlistを一つのlistにまとめる*)
  concat new_lsts

let sequence n =
  let rec loop acc i =
    if n <= i then acc else loop (run_len_encode acc) (i + 1)
  in
  let result = if n <= 0 then [] else loop [ 1 ] 1 in
  let rec convert_string lst =
    match lst with
    | [] -> ""
    | first :: rest -> string_of_int first ^ convert_string rest
  in

  convert_string result

let () =
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7);
  print_endline (sequence 8);

  print_endline "-------------------";
  print_endline (sequence 0);
  print_endline (sequence (-1));
  print_endline (sequence 1)
