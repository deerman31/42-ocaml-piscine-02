type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

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

let () =
  let a = generate_nucleotide 'A' in
  let t = generate_nucleotide 'T' in
  let c = generate_nucleotide 'C' in
  let g = generate_nucleotide 'G' in
  let none = generate_nucleotide 'X' in

  (* Test phosphate and deoxyribose *)
  assert (a.phosphate = "phosphate");
  assert (a.deoxyribose = "deoxyribose");

  (* Test nucleobase matching *)
  assert (a.nucleobase = A);
  assert (t.nucleobase = T);
  assert (c.nucleobase = C);
  assert (g.nucleobase = G);
  assert (none.nucleobase = None);

  print_endline "All tests passed!"
