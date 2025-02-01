let () = Random.self_init ()

(* EX04 *)

type phosphate = string
type deoxyribose = string

(* Uを追加*)
type nucleobase = A | T | C | G | U | None

type nucleotide = {
  phosphate : phosphate;
  deoxyribose : deoxyribose;
  nucleobase : nucleobase;
}

let generate_nucleotide t =
  {
    phosphate = "phosphate";
    deoxyribose = "deoxyribose";
    nucleobase =
      (match t with 'A' -> A | 'T' -> T | 'C' -> C | 'G' -> G | _ -> None);
  }

(* EX05 *)
type helix = nucleotide list

let generate_helix n : helix =
  let random_nucleobase () =
    match Random.int 4 with 0 -> 'A' | 1 -> 'T' | 2 -> 'C' | _ -> 'G'
  in
  let rec loop result i =
    if i <= 0 then result
    else
      let nnnn = generate_nucleotide (random_nucleobase ()) in
      loop (nnnn :: result) (i - 1)
  in
  loop [] n

let helix_to_string (h : helix) : string =
  let nucleobase_to_string (n : nucleotide) : string =
    match n with
    | { phosphate = _; deoxyribose = _; nucleobase = nn } -> (
        match nn with A -> "A" | T -> "T" | C -> "C" | G -> "G" | _ -> "")
  in

  let rec loop (result : string) (lst : helix) : string =
    match lst with
    | [] -> result
    | first :: rest -> loop (result ^ nucleobase_to_string first) rest
  in
  loop "" h

let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let complementary_helix (h : helix) : helix =
  let complementary_nucleotide n =
    {
      phosphate = "phosphate";
      deoxyribose = "deoxyribose";
      nucleobase = (match n with A -> T | T -> A | C -> G | _ -> C);
    }
  in

  let rec loop result lst =
    match lst with
    | [] -> rev result
    | { phosphate = _; deoxyribose = _; nucleobase = n } :: rest ->
        loop (complementary_nucleotide n :: result) rest
  in
  loop [] h

(* EX06 *)
type rna = nucleobase list

let generate_rna (h : helix) : rna =
  let complementary_base n =
    match n with A -> U | T -> A | C -> G | G -> C | _ -> None
  in
  let rec loop result lst =
    match lst with
    | [] -> rev result
    | { phosphate = _; deoxyribose = _; nucleobase = n } :: rest ->
        loop (complementary_base n :: result) rest
  in
  loop [] h

let () =
  let h =
    [
      { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A };
      { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T };
      { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C };
      { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G };
    ]
  in

  (* Test generate_rna *)
  let rna = generate_rna h in
  let expected = [ U; A; G; C ] in
  List.iter2 (fun n1 n2 -> assert (n1 = n2)) rna expected;

  print_endline "All tests passed!"
