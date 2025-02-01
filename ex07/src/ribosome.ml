let () = Random.self_init ()

(* EX04 *)
type phosphate = string
type deoxyribose = string
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

(* EX07 *)
type aminoacid =
  | Stop
  | Ala
  | Arg
  | Asn
  | Asp
  | Cys
  | Gln
  | Glu
  | Gly
  | His
  | Ile
  | Leu
  | Lys
  | Met
  | Phe
  | Pro
  | Ser
  | Thr
  | Trp
  | Tyr
  | Val

type protein = aminoacid list

let generate_bases_triplets (rna : rna) :
    (nucleotide * nucleotide * nucleotide) list =
  let nucleobase_to_char nucleobase =
    match nucleobase with
    | A -> 'A'
    | T -> 'T'
    | C -> 'C'
    | G -> 'G'
    | U -> 'U'
    | None -> ' '
  in

  let rec loop (result : (nucleotide * nucleotide * nucleotide) list)
      (lst : rna) =
    match lst with
    | [] -> result
    | [ _ ] | [ _; _ ] -> result
    | first :: second :: third :: rest ->
        loop
          (( generate_nucleotide (nucleobase_to_char first),
             generate_nucleotide (nucleobase_to_char second),
             generate_nucleotide (nucleobase_to_char third) )
          :: result)
          rest
  in
  loop [] rna

let rec decode_arn (rna : rna) : protein =
  let codon_to_aminoacid (nucleotide : nucleotide * nucleotide * nucleotide) :
      aminoacid option =
    match nucleotide with
    | nucleotide1, nucleotide2, nucleotide3 -> (
        match
          ( nucleotide1.nucleobase,
            nucleotide2.nucleobase,
            nucleotide3.nucleobase )
        with
        | U, A, A -> Some Stop
        | U, A, G -> Some Stop
        | U, G, A -> Some Stop
        | G, C, A -> Some Ala
        | G, C, C -> Some Ala
        | G, C, U -> Some Ala
        | A, G, A -> Some Arg
        | A, G, G -> Some Arg
        | C, G, A -> Some Arg
        | C, G, C -> Some Arg
        | C, G, G -> Some Arg
        | C, G, U -> Some Arg
        | A, A, C -> Some Asn
        | A, A, U -> Some Asn
        | G, A, C -> Some Asp
        | G, A, U -> Some Asp
        | U, G, C -> Some Cys
        | U, G, U -> Some Cys
        | C, A, A -> Some Gln
        | C, A, G -> Some Gln
        | G, A, A -> Some Glu
        | G, A, G -> Some Glu
        | G, G, A -> Some Gly
        | G, G, C -> Some Gly
        | G, G, G -> Some Gly
        | G, G, U -> Some Gly
        | C, A, C -> Some His
        | C, A, U -> Some His
        | A, U, A -> Some Ile
        | A, U, C -> Some Ile
        | A, U, U -> Some Ile
        | C, U, A -> Some Leu
        | C, U, C -> Some Leu
        | C, U, G -> Some Leu
        | C, U, U -> Some Leu
        | U, U, A -> Some Leu
        | U, U, G -> Some Leu
        | A, A, A -> Some Lys
        | A, A, G -> Some Lys
        | A, U, G -> Some Met
        | U, U, C -> Some Phe
        | U, U, U -> Some Phe
        | C, C, C -> Some Pro
        | C, C, A -> Some Pro
        | C, C, G -> Some Pro
        | C, C, U -> Some Pro
        | U, C, A -> Some Ser
        | U, C, C -> Some Ser
        | U, C, G -> Some Ser
        | U, C, U -> Some Ser
        | A, G, U -> Some Ser
        | A, G, C -> Some Ser
        | A, C, A -> Some Thr
        | A, C, C -> Some Thr
        | A, C, G -> Some Thr
        | A, C, U -> Some Thr
        | U, G, G -> Some Trp
        | U, A, C -> Some Tyr
        | U, A, U -> Some Tyr
        | G, U, A -> Some Val
        | G, U, C -> Some Val
        | G, U, G -> Some Val
        | G, U, U -> Some Val
        | _, _, _ -> None)
  in

  let rec loop (acc : protein)
      (lst : (nucleotide * nucleotide * nucleotide) list) : protein =
    match lst with
    | [] -> acc
    | first :: rest -> (
        let aminoacid = codon_to_aminoacid first in
        match aminoacid with
        | Some q -> if q = Stop then Stop :: acc else loop (q :: acc) rest
        | None -> acc)
  in

  rev (loop [] (generate_bases_triplets rna))

let string_of_protein (p : protein) : string =
  let aminoacid_to_string (a : aminoacid) : string =
    match a with
    | Stop -> "Stop"
    | Ala -> "Ala"
    | Arg -> "Arg"
    | Asn -> "Asn"
    | Asp -> "Asp"
    | Cys -> "Cys"
    | Gln -> "Gln"
    | Glu -> "Glu"
    | Gly -> "Gly"
    | His -> "His"
    | Ile -> "Ile"
    | Leu -> "Leu"
    | Lys -> "Lys"
    | Met -> "Met"
    | Phe -> "Phe"
    | Pro -> "Pro"
    | Ser -> "Ser"
    | Thr -> "Thr"
    | Trp -> "Trp"
    | Tyr -> "Tyr"
    | Val -> "Val"
  in

  let rec concat lst =
    match lst with [] -> "" | first :: rest -> first ^ concat rest
  in

  let rec loop (result : string list) (lst : protein) : string =
    match lst with
    | [] -> concat (rev result)
    | first :: rest -> loop (aminoacid_to_string first :: result) rest
  in

  loop [] p

let () =
  Random.self_init ();

  (* EX07のテスト *)
  print_endline "EX07 Tests:";

  (* テスト用のヘリックスを生成 *)
  let test_helix = generate_helix 9 in
  print_endline "Generated helix:";
  print_endline (helix_to_string test_helix);

  (* ヘリックスからRNAを生成 *)
  let test_rna = generate_rna test_helix in
  print_endline "\nGenerated RNA bases:";
  List.iter
    (fun base ->
      match base with
      | A -> print_string "A "
      | U -> print_string "U "
      | C -> print_string "C "
      | G -> print_string "G "
      | _ -> print_string "- ")
    test_rna;
  print_newline ();

  (* RNAから3塩基の組を生成 *)
  let triplets = generate_bases_triplets test_rna in
  print_endline "\nGenerated base triplets:";
  List.iter
    (fun (n1, n2, n3) ->
      Printf.printf "(%c%c%c) "
        (match n1.nucleobase with
        | A -> 'A'
        | U -> 'U'
        | C -> 'C'
        | G -> 'G'
        | _ -> '-')
        (match n2.nucleobase with
        | A -> 'A'
        | U -> 'U'
        | C -> 'C'
        | G -> 'G'
        | _ -> '-')
        (match n3.nucleobase with
        | A -> 'A'
        | U -> 'U'
        | C -> 'C'
        | G -> 'G'
        | _ -> '-'))
    triplets;
  print_newline ();

  (* RNAからタンパク質を生成 *)
  let protein = decode_arn test_rna in
  print_endline "\nGenerated protein sequence:";
  print_endline (string_of_protein protein);

  (* 既知のRNAシーケンスでのテスト *)
  let known_rna = [ A; U; G; C; C; A; U; A; A ] in
  (* Met-Pro-Stop *)
  print_endline "\nTesting known RNA sequence (AUG-CCA-UAA):";
  let known_protein = decode_arn known_rna in
  print_endline (string_of_protein known_protein)
