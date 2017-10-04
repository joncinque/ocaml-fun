open Convert

(** 256-bit output, 8 32-bit ints, 32 chars, 64 hex chars. *)

(* Helpers *)
let rec pad_char_list c l size =
  if size = 0 then l else pad_char_list c (c :: l) (size - 1)

(** Hashes by getting 32 representative characters, xors that again and again
 * against the 32 character bits, then returns that as hex. *)
let xor_hash_string s =
  let output_size = 32 in
  let xor_f = fixed_xor bin_of_ascii ascii_of_bin in
  let rec get_next_chars acc size = function
    | l when size = 0 -> (acc, l)
    | c :: cs -> get_next_chars (c :: acc) (size - 1) cs
    | [] -> (pad_char_list '_' acc size, [])
  in
  let rec do_xor result = function
    | _ :: _ as l -> begin
      let (next_chars, cs) = get_next_chars [] output_size l in
      (* Don't reverse it, make things interesting *)
      let next_xor_string = string_of_char_list next_chars in
      let new_result = xor_f result (next_xor_string) in
      do_xor new_result cs
    end
    | [] -> result
  in
  let ascii_result = do_xor (make_repeating_xor_string s output_size) (explode_string s) in
  bin_of_ascii ascii_result |> hex_of_bin

(** TODO A better hash function?
 * Hashes a string by converting 4 characters into Int32, allowing that to be
 * the first of 8 ints, then continues.  For larger inputs, will sum the character
 * onto the previous one occupying that spot.
 * At the end, converts into hex *)
