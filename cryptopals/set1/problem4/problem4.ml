open Convert
open Xor_score

let keep_best_xor_score s max_xs =
  let xs = get_best_xor_string s in
  if xs.score > max_xs.score then xs else max_xs

let () =
  let rec get_next_line f acc reader =
    match reader () with
    | None -> acc
    | Some s -> get_next_line f (f s acc) reader
  in
  let r = make_reader "4.txt" in
  get_next_line keep_best_xor_score empty_xor_score r |> print_xor_score
