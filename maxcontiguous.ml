let max_contiguous : int list -> (int * int) list = fun l ->
    (* Helper for the fold left to get the current max at each stage *)
    let get_current_max (cm, ca) x = let ca = ca + x in (max cm ca, max 0 ca)
    
    (* Get the max contiguous sum of the list *)
    in let (smax,_) = List.fold_left get_current_max (0,0) l
    
    (* In case all numbers are <= 0, simply use the max element of the list *)
    in let smax = if smax > 0 then smax else List.fold_left max min_int l
    
    (* Helper to pull out the indices whose sum add to the max sum *)
    in let get_indices (l, (cas, cae, ca), m) x =
        let ca = ca + x in
        (* Reached the max, append the current indices *)
        if ca = m then ((cas, cae)::l, (cas, cae+1, ca), m)
        (* Below 0, reset *)
        else if ca < 0 then (l, (cae+1, cae+1, 0), m)
        (* Otherwise, keep moving *)
        else (l, (cas, cae+1, ca), m)
    
    (* Do the work *)
    in let (indices,_,_) = List.fold_left get_indices ([],(0,0,0), smax) l
    in indices

(* Testing *)

(* Print helper for testing *)
let ind_print (i1,i2) = Printf.printf "(%d, %d) " i1 i2

let mc1 = max_contiguous [40;1;2;3;5;-1;-100;51;-100;40;11]
let () = List.iter ind_print mc1; Printf.printf "\n"
let mc2 = max_contiguous [40;1;2;3;5;-1;-100;52;-100;40;11]
let () = List.iter ind_print mc2; Printf.printf "\n"
let mc3 = max_contiguous [40;1;2;3;5;-1;-100;51;-100;40;12]
let () = List.iter ind_print mc3; Printf.printf "\n"
let mc4 = max_contiguous [-4; -3; -4; -100; -1]
let () = List.iter ind_print mc4; Printf.printf "\n"
let mc5 = max_contiguous []
let () = List.iter ind_print mc5; Printf.printf "\n"
