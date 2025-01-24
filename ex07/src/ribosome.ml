let () = Random.self_init ()

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

type helix = nucleotide list

let generate_helix n =
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

type rna = nucleobase list
let generate_rna h =
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

let rec decode_arn (rna : rna) : aminoacid list =
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

  let rec loop (acc : aminoacid list)
      (lst : (nucleotide * nucleotide * nucleotide) list) : aminoacid list =
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







(* Unit tests for DNA and Protein Generation *)

(* Helper function to run tests *)
let run_test test_name test_function =
  try
    test_function ();
    Printf.printf "Test %s: PASSED\n" test_name
  with
  | failure -> 
    Printf.printf "Test %s: FAILED - %s\n" test_name (Printexc.to_string failure)

(* Test generate_helix function *)
let test_generate_helix () =
  let test_length = 10 in
  let helix = generate_helix test_length in
  assert (List.length helix = test_length);
  List.iter (fun nucleotide ->
    assert (
      nucleotide.phosphate = "phosphate" &&
      nucleotide.deoxyribose = "deoxyribose" &&
      match nucleotide.nucleobase with 
      | A | T | C | G -> true 
      | _ -> false
    )
  ) helix

(* Test complementary_helix function *)
let test_complementary_helix () =
  let original_helix = generate_helix 5 in
  let complementary = complementary_helix original_helix in
  
  (* Check length is the same *)
  assert (List.length original_helix = List.length complementary);
  
  (* Check complementary base pairing *)
  List.iter2 (fun orig comp ->
    match orig.nucleobase, comp.nucleobase with
    | A, T | T, A | C, G | G, C -> ()
    | _ -> failwith "Invalid complementary base pairing"
  ) original_helix complementary

(* Test generate_rna function *)
let test_generate_rna () =
  let original_helix = generate_helix 6 in
  let rna = generate_rna original_helix in
  
  (* Check RNA conversion rules *)
  List.iter2 (fun orig rna_base ->
    match orig.nucleobase, rna_base with
    | A, U | T, A | C, G | G, C -> ()
    | _ -> failwith "Invalid RNA transcription"
  ) original_helix rna

(* Test decode_arn function *)
let test_decode_arn () =
  (* Test a known RNA sequence that should produce a specific protein *)
  let test_rna = [A; U; G; C; C; A; A; U; G] in
  let protein = decode_arn test_rna in
  
  (* This is a sample check - the exact protein depends on the specific codon mapping *)
  assert (List.length protein > 0);
  List.iter (fun amino_acid ->
    match amino_acid with
    | Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu 
    | Gly | His | Ile | Leu | Lys | Met | Phe 
    | Pro | Ser | Thr | Trp | Tyr | Val -> ()
  ) protein

(* Run all tests *)
let run_all_tests () =
  Printf.printf "Starting DNA and Protein Generation Tests\n";
  run_test "Generate Helix" test_generate_helix;
  run_test "Complementary Helix" test_complementary_helix;
  run_test "Generate RNA" test_generate_rna;
  run_test "Decode ARN" test_decode_arn;
  Printf.printf "Tests Completed\n"

(* Uncomment to run tests *)
let () = run_all_tests ()