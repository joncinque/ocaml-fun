let rec crackle_pop_print current_number end_number = 
  let number_conversion = function
    | n when n mod 15 = 0 -> "CracklePop"
    | n when n mod 5 = 0  -> "Pop"
    | n when n mod 3 = 0  -> "Crackle"
    | n                   -> string_of_int n
  in 
  let () = number_conversion current_number |> print_endline in
  if current_number = end_number then () 
  else crackle_pop_print (current_number + 1) end_number
in crackle_pop_print 1 100
