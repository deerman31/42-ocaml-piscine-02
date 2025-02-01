let rev lst =
  let rec loop acc lst =
    match lst with [] -> acc | first :: rest -> loop (first :: acc) rest
  in
  loop [] lst

let gray n =
  if n <= 0 then ()
  else
    (* 引数のdigitsのbitの最大値を取得する関数 *)
    (* digits: bitの桁数 *)
    let max_binaly_value digits =
      (* acc1: 現在の合計値, acc2: 現在のbit数, i: 現在の桁数*)
      let rec loop acc1 acc2 i =
        if i >= digits then acc1 else loop (acc1 + acc2) (acc2 * 2) (i + 1)
      in
      loop 0 1 0
    in

    (* digits桁のbitの最大値を返す関数 *)
    let max_value = max_binaly_value n in

    let pad_binary binaly bit =
      let rec loop binaly =
        if String.length binaly = bit then binaly else loop ("0" ^ binaly)
      in
      loop binaly
    in

    (* 引数の整数をbinaryに変更 *)
    let int_to_binary n =
      let rec loop acc n =
        if n <= 0 then if acc = "" then "0" ^ acc else acc
        else if n mod 2 = 1 then loop ("1" ^ acc) (n / 2)
        else loop ("0" ^ acc) (n / 2)
      in
      loop "" n
    in

    (*
グレイコードからバイナリコードへの変換をする
グレイコードからバイナリコードに変換する場合は以下の規則に従う。
最上位桁の1は同一
以下、上位から2桁ずつ見ていき、
1か0が連続していればグレイコードは0、連続していなければ1とする。
    *)
    let gray_convert binaly =
      let digits = String.length binaly in
      let string_rev s =
        let rec loop acc i =
          if i < 0 then acc
          else loop (acc ^ String.make 1 (String.get s i)) (i - 1)
        in
        loop "" (String.length s - 1)
      in

      let rec loop acc i =
        if i >= digits then string_rev acc
        else if binaly.[i - 1] = binaly.[i] then loop ("0" ^ acc) (i + 1)
        else loop ("1" ^ acc) (i + 1)
      in
      if binaly.[0] = '1' then loop "1" 1 else loop "0" 1
    in

    let rec loop acc i =
      if i > max_value then rev acc
      else
        let original_binary = int_to_binary i in

        let binaly = pad_binary original_binary n in

        let gray_num = gray_convert binaly in
        (* Printf.printf "[%d] -> [%s] -> [%s] -> [%s]\n" i original_binary binaly
           gray_num; *)
        loop (gray_num :: acc) (i + 1)
    in

    let print_lst_string lst =
      let rec loop lst =
        match lst with
        | [] -> print_newline ()
        | first :: rest ->
            print_string first;
            print_char ' ';
            loop rest
      in
      loop lst
    in
    print_lst_string (loop [] 0)

let () =
  gray 1;
  gray 2;
  gray 3
