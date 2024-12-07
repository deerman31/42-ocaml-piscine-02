let run_length_encode seq =
  let len = String.length seq in

  let rec loop result current i =
    if i >= len then
      result ^ string_of_int (String.length current) ^ String.make 1 current.[0]
    else if current.[0] = seq.[i] then
      loop result (current ^ String.make 1 seq.[i]) (i + 1)
    else
      loop
        (result
        ^ string_of_int (String.length current)
        ^ String.make 1 current.[0])
        (String.make 1 seq.[i])
        (i + 1)
  in

  loop "" (String.make 1 seq.[0]) 1

let sequence n =
  let rec loop acc i =
    if i >= n then acc else loop (run_length_encode acc) (i + 1)
  in
  if n <= 0 then "" else loop "1" 1
;;

print_endline (sequence 1);
print_endline (sequence 2);
print_endline (sequence 3);
print_endline (sequence 4);
print_endline (sequence 5);
print_endline (sequence 6);
print_endline (sequence 7)
