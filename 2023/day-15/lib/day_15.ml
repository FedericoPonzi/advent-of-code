
let hash_algorithm str =
  let rec hash_helper current_value = function
    | [] -> current_value
    | c :: rest ->
      let ascii_code = Char.code c in
      let new_value = (current_value + ascii_code) * 17 mod 256 in
      hash_helper new_value rest
  in
  hash_helper 0 (List.init (String.length str) (String.get str))

let result = hash_algorithm "HASH";;

