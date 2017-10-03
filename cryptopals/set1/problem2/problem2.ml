open Convert
open Big_int

(* Problem 2, first attempt *)
let big_int_of_hex_deprecated s =
  let hex_binary_size = 4 in
  let rec add_next_char acc = function
    | c :: cs -> begin
      let a = shift_left_big_int acc hex_binary_size in
      let b = int_of_hex_char c |> big_int_of_int in
      add_next_char (add_big_int a b) cs
    end
    | [] -> acc
  in add_next_char zero_big_int (explode_string s)

let hex_of_big_int_deprecated i = 
  let hex_size = big_int_of_int 16 in
  let rec add_hex_val acc a = 
    if eq_big_int a zero_big_int then acc
    else begin
      let (q,r) = quomod_big_int a hex_size in
      add_hex_val ((int_of_big_int r |> hex_char_of_int) :: acc) q
    end
  in
  let t = add_hex_val [] i in
  let () = List.iter (Printf.printf "%c") t in
  string_of_char_list t

let fixed_xor_deprecated rh lh =
  let r = big_int_of_hex_deprecated rh in
  let l = big_int_of_hex_deprecated lh in
  xor_big_int r l |> hex_of_big_int_deprecated

let () = 
  let () = print_endline "Testing xor for hex" in
  let t1 = "1c0111001f010100061a024b53535009181c" in 
  let t2 = "686974207468652062756c6c277320657965" in 
  let res = "746865206b696420646f6e277420706c6179" in
  let x = fixed_xor bin_of_hex hex_of_bin t1 t2 in
  let () = print_endline x in
  if x = res then print_endline "Success" else print_endline "Failure"

