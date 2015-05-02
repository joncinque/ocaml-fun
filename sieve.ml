let rec primes : int -> int list = fun limit ->

    (* simple helper function to create the list of numbers to sieve through *)
    let rec generate_list : int list -> int -> int list = fun acc limit ->
        if limit > 1 then generate_list (limit::acc) (limit-1) else acc
    in let candidate_list = generate_list [] limit
    
    (* main work to recursively build up the list of primes *)
    in let rec filter_candidate_list : int list -> int list -> int list =
        fun prime_list -> function
        
        (* the head of the list is prime because it got through all previous 
         * filtering stages, so append it to the list of primes *)
        | hd::tl -> filter_candidate_list (hd::prime_list)
                    (* filter out the multiples of the current prime hd from the
                     * rest of the list *)
                    (List.filter (fun x -> (x mod hd) != 0) tl )
                    
        (* at the end of the list, just return *)
        | _  -> prime_list
        
    in List.rev(filter_candidate_list [] candidate_list)
    
(* Testing *)
let primesBelow121 = primes 121
let actualPrimesBelow121 = [ 2;3;5;7;11;13;17;19;23;29;31;37;41;43;47;53;59;61;67;71;73;79;83;89;97;101;103;107;109;113 ]
(* make sure all elements are the same *)
let allOk = List.fold_left2 (fun allOk x y -> allOk && (x==y)) true
                            primesBelow121 actualPrimesBelow121
let () = assert(allOk == true)
