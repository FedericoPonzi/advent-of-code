let hash_algorithm str =
  let rec hash_helper current_value = function
    | [] -> current_value
    | c :: rest ->
      let ascii_code = Char.code c in
        let new_value = (current_value + ascii_code) * 17 mod 256 in
          hash_helper new_value rest
  in
  hash_helper 0 (List.init (String.length str) (String.get str))

(*let result = hash_algorithm "HASH";;*)

let hash_fun_and_accumulate sum str =
  let hash_result = hash_algorithm str in
  sum + hash_result

let solve1 filename =
  let channel = open_in filename in
    let final_sum =
      let line = input_line channel in
        let values = String.split_on_char ',' line in
          let new_sum = List.fold_left (hash_fun_and_accumulate) 0 values in
            let () = close_in channel in new_sum
    in
      final_sum

let () =
  let filename = "example.txt" in
  ignore(Printf.printf "Example, received: %d, expected: 1320\n" (solve1 filename ))

let () =
  let filename = "input.txt" in
  ignore(Printf.printf "Solve1, received: %d, expected: 513643\n" (solve1 filename ))
