open Convert
open Xor_score

(* First attempt *)
let ascii_of_hex_deprecated h =
  let ascii_of_hex_chars ah bh =
    let a = int_of_hex_char ah in
    let b = int_of_hex_char bh in
    (a + b lsl 4) |> char_of_int
  in
  let rec add_ascii_vals acc = function
    | [] -> acc
    | a :: b :: l -> add_ascii_vals ((ascii_of_hex_chars a b) :: acc) l
    | _ -> raise (Invalid_argument "Uneven hex string")
  in 
  explode_string h |> List.rev |> add_ascii_vals [] |> string_of_char_list

(* Tests *)
let () =
  let () = print_endline "Testing ascii<->bin" in
  let h = "5468697320697320736f6d6520746578742e" in
  let t = "This is some text." in
  let b = "010101000110100001101001011100110010000001101001011100110010000001110011011011110110110101100101001000000111010001100101011110000111010000101110" in
  let a = ascii_of_hex h in
  let () = if a = t then print_endline "Success" else print_endline "Failure" in
  let bt = bin_of_ascii t in
  let () = if bt = b then print_endline "Success" else print_endline "Failure" in
  let bh = hex_of_bin bt in
  let () = if bh = h then print_endline "Success" else print_endline "Failure" in
  let ta = ascii_of_bin bt in
  let () = if ta = t then print_endline "Success" else print_endline "Failure" in
  let (s,r) = get_best_xor_string "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736" in
  print_endline r
