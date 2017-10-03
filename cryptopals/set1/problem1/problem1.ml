open Convert

(* Problem 1, first attempt *) 
let base64_of_hex_deprecated h =
  let convert_3_hex acc ah bh ch = 
    let a = int_of_hex_char ah in
    let b = int_of_hex_char bh in
    let c = int_of_hex_char ch in
    let first = a * 4 + b lsr 2 |> base64_of_int in
    let second = (b land 3) lsl 4 + c |> base64_of_int in
    second :: first :: acc
  in 
  let convert_2_hex acc ah bh = 
    let a = int_of_hex_char ah in
    let b = int_of_hex_char bh in
    let first = a * 4 + b lsr 2 |> base64_of_int in
    let second = b land 3 |> base64_of_int in
    second :: first :: acc
  in
  let convert_1_hex acc ah =
    let first = int_of_hex_char ah |> base64_of_int in
    first :: acc
  in
  let rec get_next_chars acc = function
    | a :: b :: c :: l -> get_next_chars (convert_3_hex acc a b c) l 
    | a :: b :: [] -> convert_2_hex acc a b
    | a :: [] -> convert_1_hex acc a
    | [] -> acc
  in
  get_next_chars [] (explode_string h) |> List.rev |> string_of_char_list

(* Tests *)
let () =
  let () = print_endline "Testing base64<->hex" in
  let b = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let h = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let bsf = base64_of_hex h in
  if bsf = b then print_endline "Success" else print_endline "Failure" 

let () = 
  let () = print_endline "Testing hex<->bin" in
  let h = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d" in
  let b = "010010010010011101101101001000000110101101101001011011000110110001101001011011100110011100100000011110010110111101110101011100100010000001100010011100100110000101101001011011100010000001101100011010010110101101100101001000000110000100100000011100000110111101101001011100110110111101101110011011110111010101110011001000000110110101110101011100110110100001110010011011110110111101101101" in
  let big_bin = bin_of_hex h in
  let () = if big_bin = b then print_endline "Success" else print_endline "Failure" in
  let bin_conv = hex_of_bin big_bin in
  if bin_conv = h then print_endline "Success" else print_endline "Failure"

let () =
  let () = print_endline "Testing base64<->bin" in
  let s = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t" in
  let b = "010010010010011101101101001000000110101101101001011011000110110001101001011011100110011100100000011110010110111101110101011100100010000001100010011100100110000101101001011011100010000001101100011010010110101101100101001000000110000100100000011100000110111101101001011100110110111101101110011011110111010101110011001000000110110101110101011100110110100001110010011011110110111101101101" in
  let t = bin_of_base64 s in
  let () = if t = b then print_endline "Success" else print_endline "Failure" in
  let b64 = base64_of_bin t in
  if b64 = s then print_endline "Success" else print_endline "Failure"

