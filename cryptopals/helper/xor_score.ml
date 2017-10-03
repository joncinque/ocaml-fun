open Convert

(* Problem 3 and 4 *)

(** Helper for reading files line by line *)
let make_reader file_name =
  let ic = open_in file_name in
  let closed = ref false in
  let read_next_line () =
    if !closed then None
    else begin
      try
        Some (input_line ic)
      with
        End_of_file ->
          let _ = close_in_noerr ic in
          let _ = closed := true in
          None
    end
  in read_next_line

type xor_score = { score: int ; result: string }

let empty_xor_score = { score = 0; result = "" }

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

let print_xor_score xs =
  print_int xs.score; print_char ' '; print_string xs.result

let get_best_xor_string xor_string =
  let rec test_string s l max_xs = function
    | 256 -> max_xs
    | i -> begin
      let t = String.make l (Char.chr i) in
      let res = fixed_xor bin_of_ascii ascii_of_bin s t in
      let score = score_string res in
      if score > max_xs.score then test_string s l {score = score; result = res } (i+1)
      else test_string s l max_xs (i+1)
    end
  in
  let a_string = ascii_of_hex xor_string in
  let len = String.length a_string in
  test_string a_string len empty_xor_score 0

