
(* Functional helpers *)
let identity i = i
let (>>) f g x = f x |> g
let (<<) f g x = f (g x)

let string_of_char_list l =
  let buf = Buffer.create (List.length l) in
  let () = List.iter (Buffer.add_char buf) l in
  Buffer.contents buf

let explode_string s =
  let rec push i l =
    if i < 0 then l else push (i-1) (s.[i] :: l) in
  push (String.length s - 1) []

(* Problem 1 *)
let int_of_hex_char = function
  | c when 48 <= Char.code c && Char.code c <= 57 ->  Char.code c - 48 (* 0-9, 0-9 *)
  | c when 65 <= Char.code c && Char.code c <= 70 -> Char.code c - 55  (* A-F, 10-15 *)
  | c when 97 <= Char.code c && Char.code c <= 102 -> Char.code c - 87 (* a-f, 10-15 *)
  | _ -> raise (Invalid_argument "Character out of hex range")

let int_of_base64_char = function
  | c when 65 <= Char.code c && Char.code c <= 90 -> Char.code c - 65 (* A-Z, 0-25 *)
  | c when 97 <= Char.code c && Char.code c <= 122 -> Char.code c - 71 (* a-z, 26-51 *)
  | c when 48 <= Char.code c && Char.code c <= 57 -> Char.code c + 4 (* 0-9, 52-61 *)
  | '+' -> 62
  | '/' -> 63
  | _ -> raise (Invalid_argument "Int value out of range")

let base64_of_int = function
  | i when 0 <= i && i < 26 -> Char.chr (i + 65) (* A-Z *)
  | i when i < 52 -> Char.chr (i + 71) (* a-z *)
  | i when i < 62 -> Char.chr (i - 4) (* 0-9 *)
  | 62 -> '+'
  | 63 -> '/'
  | _ -> raise (Invalid_argument "Int value out of range")

let hex_char_of_int = function
  | a when 0 <= a && a < 10 -> Char.chr (a + 48) (* 0-9 *)
  | a when a < 16 -> Char.chr (a + 87) (* a-f *)
  | _ -> raise (Invalid_argument "Input too large")

let char_of_bin = function
  | 0 -> '0'
  | 1 -> '1'
  | _ -> raise (Invalid_argument "Input too large")

let bin_of_char = function
  | '0' -> 0
  | '1' -> 1
  | _ -> raise (Invalid_argument "Input too large")

(** Conversion function to create a list of binary characters from some a list
 * of characters in another basis.
 * Parameters:
   * f_conv: conversion function from one character to int
   * b_size: the number of bits per character 
   * l: list of characters to convert *)
let to_bits f_conv b_size l =
  let rec fill_zeroes l num =
    if num = 0 then l else fill_zeroes ('0' :: l) (num - 1)
  in
  let rec add_bin_chars_of_int l n = function
    | 0 -> fill_zeroes l n
    | a -> add_bin_chars_of_int ((a mod 2|> char_of_bin) :: l) (n - 1) (a / 2)
  in
  let rec add_bin_chars_of_char f_convert b_size acc = function
    | x :: xs -> begin
      let int_val = f_convert x in
      let acc_next = add_bin_chars_of_int acc b_size int_val in
      add_bin_chars_of_char f_convert b_size acc_next xs
    end
    | [] -> acc
  in add_bin_chars_of_char f_conv b_size [] l

(** Conversion function to work with up to 8 binary characters. *)
let int_of_bin f_convert a_bin b_bin c_bin d_bin e_bin f_bin g_bin h_bin =
  let a = bin_of_char a_bin and b = bin_of_char b_bin and c = bin_of_char c_bin
  and d = bin_of_char d_bin and e = bin_of_char e_bin and f = bin_of_char f_bin
  and g = bin_of_char g_bin and h = bin_of_char h_bin in
  let res = (a lsl 7) + (b lsl 6) + (c lsl 5) + (d lsl 4) + (e lsl 3) + (f lsl 2) + (g lsl 1) + h in
  f_convert res

(** Conversion function to work with up to 4 ascii characters. *)
let int_of_chars f_convert c1 c2 c3 c4 =
  let i1 = Char.code c1 and i2 = Char.code c2 and i3 = Char.code c3 and i4 = Char.code c4
  in  (i1 lsl 24) + (i2 lsl 16) + (i3 lsl 8) + i4 |> f_convert

(** The main conversion function from a list of bits to a list of anything else.
 * Parameters:
   * f_conv: a conversion function from int, e.g. char_of_int
   * size: how many bits per character, max 8
   * l: a list of bit characters to be converted
 * It uses a combination of match patterns and number of bits for each character. 
 * Both parameters are used when matching, so that it will still function even 
 * if the input list does not have the right amount of characters for the
 * desired size.
 * For example, giving a list of 10 bits and converting to ASCII leaves 2 free
 * bits, which are caught by a different match pattern.
 *)
let of_bits f_conv size l =
  let rec add_vals f_conv size acc = function
    | a :: b :: c :: d :: e :: f :: g :: h :: xs when size >= 8 -> 
        add_vals f_conv size ((int_of_bin f_conv a b c d e f g h)::acc) xs
    | a :: b :: c :: d :: e :: f :: g :: xs when size >= 7 -> begin
      let conv_char = int_of_bin f_conv '0' a b c d e f g in
      if size = 7 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: b :: c :: d :: e :: f :: xs when size >= 6 -> begin
      let conv_char = int_of_bin f_conv '0' '0' a b c d e f in
      if size = 6 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: b :: c :: d :: e :: xs when size >= 5-> begin
      let conv_char = int_of_bin f_conv '0' '0' '0' a b c d e in
      if size = 5 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: b :: c :: d :: xs when size >= 4 -> begin
      let conv_char = int_of_bin f_conv '0' '0' '0' '0' a b c d in
      if size = 4 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: b :: c :: xs when size >= 3 -> begin
      let conv_char = int_of_bin f_conv '0' '0' '0' '0' '0' a b c in
      if size = 3 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: b :: xs when size >= 2 -> begin
      let conv_char = int_of_bin f_conv '0' '0' '0' '0' '0' '0' a b in
      if size = 2 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | a :: xs when size >= 1 -> begin
      let conv_char = int_of_bin f_conv '0' '0' '0' '0' '0' '0' '0' a in
      if size = 1 then add_vals f_conv size (conv_char::acc) xs else conv_char::acc
    end
    | [] -> acc
    | _ -> raise (Invalid_argument "Something went wrong")
  in add_vals f_conv size [] l

let bin_of_hex =
  explode_string >> List.rev >> to_bits int_of_hex_char 4 >> string_of_char_list

let bin_of_base64 =
  explode_string >> List.rev >> to_bits int_of_base64_char 6 >> string_of_char_list

let bin_of_ascii = 
  explode_string >> List.rev >> to_bits Char.code 8 >> string_of_char_list

let hex_of_bin = 
  explode_string >> of_bits hex_char_of_int 4 >> List.rev >> string_of_char_list

let base64_of_bin =
  explode_string >> of_bits base64_of_int 6 >> List.rev >> string_of_char_list

let ascii_of_bin = 
  explode_string >> of_bits char_of_int 8 >> List.rev >> string_of_char_list

let convert_base input_size output_size = 
  let to_bin_func = match input_size with
  | 4 -> bin_of_hex
  | 6 -> bin_of_base64
  | 8 -> bin_of_ascii
  | _ -> raise (Invalid_argument "Base out of range")
  in
  let of_bin_func = match output_size with
  | 4 -> hex_of_bin
  | 6 -> base64_of_bin
  | 8 -> ascii_of_bin
  | _ -> raise (Invalid_argument "Base out of range")
  in to_bin_func >> of_bin_func

let ascii_of_hex = bin_of_hex >> ascii_of_bin

let base64_of_hex = bin_of_hex >> base64_of_bin

(* Problem 2 *)
let fixed_xor conv1 conv2 rh lh =
  let xor_chars ref_string i c = if c = ref_string.[i] then '0' else '1' in
  let r = conv1 rh in
  let l = conv1 lh in
  if String.length r != String.length l then raise (Invalid_argument "Lengths must be equal")
  else String.mapi (xor_chars l) r |> conv2

(* Problem 3
 * A hex encoded string has been XOR'd against a single character
 * repeated many times. Find the key, decrypt the message. *)
let score_string s =
  let score_char c = match Char.uppercase_ascii c with
    | 'E' -> 21
    | 'T' -> 16
    | 'A' -> 14
    | 'O' -> 14
    | 'I' -> 13
    | 'N' -> 12
    | ' ' -> 11
    | 'S' -> 11
    | 'R' -> 10
    | 'H' -> 10
    | 'D' -> 7
    | 'L' -> 7
    | 'U' -> 5
    | 'C' -> 4
    | 'M' -> 4
    | 'F' -> 4
    | 'Y' -> 3
    | 'W' -> 3
    | 'G' -> 3
    | 'P' -> 3
    | 'B' -> 2
    | 'V' -> 2
    | 'K' -> 1
    | 'X' -> 1
    | 'Q' -> 1
    | 'J' -> 1
    | 'Z' -> 1
    | '-' -> 1
    | '\'' -> 1
    | _   -> -10
  in
  let score_acc acc c = acc + (score_char c) in
  List.fold_left score_acc 0 (explode_string s)

let ascii_of_hex = bin_of_hex >> ascii_of_bin

let make_repeating_xor_string s output_size = 
  let char_list = explode_string s in
  let rec add_char acc size = function
    | _ when size = 0 -> acc
    | c :: cs -> add_char (c :: acc) (size - 1) cs
    | [] when size > 0 -> add_char acc size char_list (* start again! *)
    | [] -> acc
  in
  add_char [] output_size char_list |> List.rev |> string_of_char_list

let get_best_xor_string xor_string =
  let rec test_string s l m_score m_res = function
    | 256 -> m_res
    | i -> begin
      let t = String.make l (Char.chr i) in
      let res = fixed_xor bin_of_ascii ascii_of_bin s t in
      let score = score_string res in
      if score > m_score then test_string s l  score res (i+1)
      else test_string s l  m_score m_res (i+1)
    end
  in
  let a_string = ascii_of_hex xor_string in
  let len = String.length a_string in
  test_string a_string len 0 "" 0


